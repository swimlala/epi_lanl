CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:08:54Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170854  20220204114411  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؅��5�1   @؅�q�+&@7ffffff�c�����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB'��B0  B8  B@  BH  BPffBX  B`  Bh  Bp  Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC�fDD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DP��DQ� DR  DR� DS  DS�fDT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\�fD]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DtfDt� Dt� Dy��D� �D�UqD���D�ӅD���D�N�D��HD��D� D�XRD���Dǌ�D�!�D�Y�Dژ�D�\D��D�^�D�D��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@���@���Az�A<z�A\z�A|z�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B�B�B�B&�RB/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bv�RB�B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\BÏ\BǏ\Bˏ\BϏ\Bӏ\B׏\Bۏ\Bߏ\B�\B�\B�\B�\B�\B��\B��\B�CǮCǮCǮCǮC	ǮCǮCǮCǮCǮCǮCǮCǮCǮCǮCǮCǮC!ǮC#ǮC%ǮC'ǮC)ǮC+�HC-ǮC/ǮC1ǮC3ǮC5ǮC7ǮC9ǮC;ǮC=ǮC?ǮCAǮCCǮCEǮCGǮCIǮCKǮCMǮCOǮCQǮCSǮCUǮCWǮCYǮC[ǮC]ǮC_ǮCaǮCcǮCeǮCgǮCiǮCk�HCmǮCoǮCqǮCsǮCuǮCwǮCyǮC{ǮC}ǮCǮC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C��
C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��
C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D q�D ��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��D	q�D	��D
q�D
��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dk�D��Dq�D��Dq�D��D q�D ��D!q�D!��D"q�D"��D#q�D#��D$q�D$��D%q�D%��D&q�D&��D'q�D'��D(q�D(��D)q�D)��D*q�D*��D+q�D+��D,q�D,��D-q�D-��D.q�D.��D/q�D/��D0q�D0��D1q�D1��D2q�D2��D3q�D3��D4q�D4��D5q�D5��D6q�D6��D7q�D7��D8q�D8��D9q�D9��D:q�D:��D;q�D;��D<q�D<��D=q�D=��D>q�D>��D?q�D?��D@q�D@��DAq�DA��DBq�DB��DCxRDC��DDq�DD��DEq�DE��DFq�DF��DGq�DG��DHq�DH��DIq�DI��DJq�DJ��DKq�DK��DLq�DL��DMq�DM��DNq�DN��DOq�DO��DPq�DP�DQq�DQ��DRq�DR��DSxRDS��DTq�DT��DUq�DU��DVq�DV��DWq�DW��DXq�DX��DYq�DY��DZq�DZ��D[q�D[��D\xRD\��D]q�D]��D^q�D^��D_q�D_��D`q�D`��Daq�Da��Dbq�Db��Dcq�Dc��Ddq�Dd��Deq�De��Dfq�Df��Dgq�Dg��Dhq�Dh��Diq�Di��Djq�Dj��Dkq�Dk��Dlq�Dl��Dmq�Dm��Dnq�Dn��Doq�Do��Dpq�Dp��Dqq�Dq��Drq�Dr��Dsq�Ds�RDtq�Dt��Dy�{D��D�NgD���D��{D���D�G�D��>D��D��D�QHD���Dǅ�D��D�R�Dڑ�D�RD�{D�W�D�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AԮAԶFAԶFAԼjAԼjA���A���AԾwA�ĜA���A�ĜA���A�ȴAԩ�A�bNA�K�A�oA�v�A�/A�t�A���A�A�A��A�1A���Aė�A�?}A�jA�"�A��
A��DA��#A���A�r�A�+A��A��A�9XA�  A�+A���A��A�%A��mA�"�A�|�A� �A��A��A�C�A��hA��PA�9XA�33A� �A��A���A���A�r�A�E�A�\)A�E�A� �A��-A�1'A�G�A�-A���A�x�A�;dA��A�A�G�A�oA���A��FA���A��A�ZA���A�A�1'A�G�A��#A���A�ĜA�dZA��\A���A���A�\)A�O�A� �A���A�7LA��A�dZA��RA�p�A�n�A���A�1A}�Az�jAz1Ay��Aw`BAtv�Ar�uAr(�Ao�
AnZAm�Ak�mAk+Aj1'AhZAg33Af�Ae�wAdȴAbA�A`ĜA_��A]�A\��AY��AXZAW�^AV�/AVVAU|�AR��AQl�AO�wANVAL�AK�;AKAI�hAG��AF��AE�;ADM�AC"�AB��AAS�A?XA>��A>�A=�;A=�PA<�DA:�/A:VA:  A9�A97LA9�A9
=A8�jA89XA7K�A6�jA69XA5�TA5�^A5�A4�A333A2 �A0�uA/��A-hsA,��A,bNA,  A+hsA*��A*��A*ffA)��A)�A)%A(�A(��A'��A&�jA%7LA$Q�A#�A!p�A!A ��A �RA ZA33A��AƨA��A�A\)A�/A��AbNAȴA�-A~�A&�A��A�/A��A�`A/A �A	��AAQ�A��AȴAbAA��A  AhsA ^5@�?}@�^5@��@��T@��-@�O�@�b@�M�@�A�@�K�@���@�G�@�
=@�n�@��@�&�@��@� �@�t�@�?}@���@��@�-@���@�?}@�Q�@�\)@��@�h@��@��@�@�\)@�X@�b@�|�@ָR@Ցh@���@�1'@��@ЋD@��@�=q@�A�@ʗ�@�r�@��@���@�%@�A�@��y@�@���@�  @�S�@��T@�(�@�^5@�1'@��w@��@��@���@�~�@�V@�I�@��F@�33@���@�=q@���@���@�@��#@��h@���@�b@�dZ@�ȴ@�E�@���@��9@��w@��@�@�%@��P@���@�ȴ@��!@��!@���@�ff@�@�@��7@�hs@���@�A�@��;@��w@�|�@�S�@��@��H@��H@�o@�33@���@�M�@���@�x�@���@�?}@�j@���@��P@�t�@�S�@�o@�"�@�33@��@�-@�@�@�J@�$�@���@��7@�p�@�x�@��7@�p�@�7L@���@��@�Z@�  @���@��F@�S�@��w@�33@�5?@��T@��^@��-@�hs@���@���@��9@�1'@�  @� �@�A�@�r�@�Ĝ@���@�Ĝ@��@�Q�@�(�@��@��w@��P@�t�@���@��\@�V@��-@�$�@�X@��@�%@�V@�V@��`@��@�r�@�I�@�(�@��@���@�t�@�t�@�S�@�S�@�33@��R@�ff@�{@�@�&�@��@��@�V@���@���@�Ĝ@�Ĝ@��j@���@��@�(�@���@�S�@��@��R@���@�v�@�ff@�^5@�=q@�J@��^@��^@�@��^@���@��@�`B@�G�@��j@�j@�A�@�b@��
@��@���@�S�@�o@�o@���@�ff@��@�@���@���@���@��7@��@�X@��@���@���@��j@�z�@�A�@�(�@�1@��m@���@�C�@�o@���@���@��\@�v�@�^5@�M�@��@�,=@xbN@qj@i�@_�@Wg�@O�@H�5@D�4@=��@8M@3F�@- \@'s@!�N@<6@�@4n@ƨ@t�@Ft111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AԮAԶFAԶFAԼjAԼjA���A���AԾwA�ĜA���A�ĜA���A�ȴAԩ�A�bNA�K�A�oA�v�A�/A�t�A���A�A�A��A�1A���Aė�A�?}A�jA�"�A��
A��DA��#A���A�r�A�+A��A��A�9XA�  A�+A���A��A�%A��mA�"�A�|�A� �A��A��A�C�A��hA��PA�9XA�33A� �A��A���A���A�r�A�E�A�\)A�E�A� �A��-A�1'A�G�A�-A���A�x�A�;dA��A�A�G�A�oA���A��FA���A��A�ZA���A�A�1'A�G�A��#A���A�ĜA�dZA��\A���A���A�\)A�O�A� �A���A�7LA��A�dZA��RA�p�A�n�A���A�1A}�Az�jAz1Ay��Aw`BAtv�Ar�uAr(�Ao�
AnZAm�Ak�mAk+Aj1'AhZAg33Af�Ae�wAdȴAbA�A`ĜA_��A]�A\��AY��AXZAW�^AV�/AVVAU|�AR��AQl�AO�wANVAL�AK�;AKAI�hAG��AF��AE�;ADM�AC"�AB��AAS�A?XA>��A>�A=�;A=�PA<�DA:�/A:VA:  A9�A97LA9�A9
=A8�jA89XA7K�A6�jA69XA5�TA5�^A5�A4�A333A2 �A0�uA/��A-hsA,��A,bNA,  A+hsA*��A*��A*ffA)��A)�A)%A(�A(��A'��A&�jA%7LA$Q�A#�A!p�A!A ��A �RA ZA33A��AƨA��A�A\)A�/A��AbNAȴA�-A~�A&�A��A�/A��A�`A/A �A	��AAQ�A��AȴAbAA��A  AhsA ^5@�?}@�^5@��@��T@��-@�O�@�b@�M�@�A�@�K�@���@�G�@�
=@�n�@��@�&�@��@� �@�t�@�?}@���@��@�-@���@�?}@�Q�@�\)@��@�h@��@��@�@�\)@�X@�b@�|�@ָR@Ցh@���@�1'@��@ЋD@��@�=q@�A�@ʗ�@�r�@��@���@�%@�A�@��y@�@���@�  @�S�@��T@�(�@�^5@�1'@��w@��@��@���@�~�@�V@�I�@��F@�33@���@�=q@���@���@�@��#@��h@���@�b@�dZ@�ȴ@�E�@���@��9@��w@��@�@�%@��P@���@�ȴ@��!@��!@���@�ff@�@�@��7@�hs@���@�A�@��;@��w@�|�@�S�@��@��H@��H@�o@�33@���@�M�@���@�x�@���@�?}@�j@���@��P@�t�@�S�@�o@�"�@�33@��@�-@�@�@�J@�$�@���@��7@�p�@�x�@��7@�p�@�7L@���@��@�Z@�  @���@��F@�S�@��w@�33@�5?@��T@��^@��-@�hs@���@���@��9@�1'@�  @� �@�A�@�r�@�Ĝ@���@�Ĝ@��@�Q�@�(�@��@��w@��P@�t�@���@��\@�V@��-@�$�@�X@��@�%@�V@�V@��`@��@�r�@�I�@�(�@��@���@�t�@�t�@�S�@�S�@�33@��R@�ff@�{@�@�&�@��@��@�V@���@���@�Ĝ@�Ĝ@��j@���@��@�(�@���@�S�@��@��R@���@�v�@�ff@�^5@�=q@�J@��^@��^@�@��^@���@��@�`B@�G�@��j@�j@�A�@�b@��
@��@���@�S�@�o@�o@���@�ff@��@�@���@���@���@��7@��@�X@��@���@���@��j@�z�@�A�@�(�@�1@��m@���@�C�@�o@���@���@��\@�v�@�^5@�M�G�O�@�,=@xbN@qj@i�@_�@Wg�@O�@H�5@D�4@=��@8M@3F�@- \@'s@!�N@<6@�@4n@ƨ@t�@Ft111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B�B�
B�B�
B�
B�B�
B�B�B�NB�mB�B�B�BB\B�B,BC�Bz�B|�Bn�BBȴB��B�}B��B�1B�B��B�RB��BÖBƨBŢBĜB��B�qB�jB�XB�RB�3B�B�B�B��B��B��B��B�\B�VB�PB�7B�B~�Bw�BiyBbNBYBI�B33B�BJB�B�)B��BÖB�'B�B��B��B��B��B��B~�Bw�Br�Bq�Bo�B`BBYBXBXBS�BE�B+B�BB
��B
�NB
�B
��B
��B
ɺB
�qB
�XB
�B
��B
�B
|�B
bNB
[#B
W
B
F�B
33B
 �B
�B
\B	��B	��B	�B	�`B	�BB	��B	��B	��B	ɺB	ĜB	�}B	�9B	�-B	��B	��B	�{B	�1B	�B	�B	{�B	x�B	m�B	dZB	]/B	W
B	P�B	J�B	E�B	>wB	6FB	.B	)�B	#�B	�B	�B	{B	DB	+B	B	B	  B��B��B�B�B�B�B�B�B�B�sB�fB�NB�BB�5B�/B�#B�B��B��BŢBB�dB�FB�?B�?B�'B�!B�B�B��B��B��B��B��B��B��B��B��B�{B�VB�DB�=B�7B�1B�B�B|�By�Bx�B}�B|�B|�By�Bv�Bs�Bq�Bn�Bk�BgmBffBe`B^5B\)BW
BP�BO�BJ�BJ�BG�BE�BD�BB�BA�B?}B>wB<jB:^B:^B:^B9XB8RB6FB5?B49B33B33B33B1'B1'B1'B0!B/B.B/B-B.B-B,B-B-B,B,B,B1'B1'B49B8RB8RB9XB:^B<jB?}B@�BA�BB�BH�BG�BF�BI�BK�BO�BP�BS�BVBYB^5B`BBcTBe`BffBjBl�Bo�Bp�Bq�Br�Br�Br�Br�Bv�Bv�Bw�Bx�Bz�B{�B|�B~�B�B�+B�1B�JB�VB�bB�oB�{B��B��B��B��B��B��B�B�B�B�B�B�B�B�-B�3B�?B�FB�XB�qB��BÖBƨB��B��B��B��B��B��B��B��B��B�B�)B�5B�5B�5B�5B�5B�HB�mB�yB�B�B��B��B��B	B	+B		7B	PB	VB	\B	bB	�B	�B	�B	�B	�B	�B	 �B	#�B	%�B	+B	49B	:^B	@�B	A�B	A�B	A�B	C�B	E�B	I�B	I�B	I�B	K�B	M�B	P�B	T�B	XB	\)B	_;B	bNB	cTB	bNB	cTB	ffB	gmB	l�B	l�B	l�B	s�B	{�B	~�B	�B	�+B	�+B	�=B	�JB	�PB	�VB	�VB	�PB	�VB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�3B	�9B	�FB	�LB	�XB	�dB	�jB	�}B	��B	��B	��B	ÖB	ĜB	ƨB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�/B	�5B	�5B	�;B	�BB	�BB	�HB	�NB	�TB	�TB	�TB	�ZB	�`B	�`B	�`B	�`B	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B
B
	RB
B
�B
'�B
0�B
5�B
="B
BB
G�B
L�B
SB
VB
]�B
a�B
f�B
lqB
n}B
r�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B��B��B��B�B�B�B�B�B�B�B�B�B�JB�hB�B�B�B� BUB�B# B:�Bq�Bs�Be�B�B��BüB�nB��B&BzB��B�EB�uB��B��B��B��B�}B�eB�^B�MB�GB�)B�
B�B��B��B��B��B��B�UB�OB�JB�1BxBu�Bn�B`vBYLBPB@�B*4B�BOB�B�2B�B��B�3B� B��B��B��B��B��BvBn�Bi�Bh�Bf�BWUBP+BO$BO$BKB<�B"B�B
�4B
��B
�lB
�;B
�B
��B
��B
��B
�yB
�*B
��B
|DB
tB
YvB
RLB
N3B
=�B
*_B
�B
�B
�B	�B	�B	�B	ܓB	�uB	�2B	�B	��B	��B	��B	��B	�pB	�eB	�B	�B	��B	lB	{UB	xBB	s#B	pB	d�B	[�B	TnB	NJB	H&B	BB	<�B	5�B	-�B	%XB	!@B	B	�B	�B	�B	�B�sB�aB�TB�HB�BB�B��B��B��B��B��B��B��B߽BݱBٙB׍BՀB�{B�oB�cB�9B� B��B��B��B��B��B��B�wB�qB�^B�XB�MB�AB�:B�4B�.B�"B�B��B��B��B��B��B��B��B�B{nBx[BtCBq1Bp+BuJBtDBtDBq1Bn BkBiBe�Bb�B^�B]�B\�BU�BS�BNeBHABG;BBBBB?B<�B;�B9�B8�B6�B5�B3�B1�B1�B1�B0�B/�B-�B,�B+�B*�B*�B*�B(�B(�B(�B'�B&|B%uB&|B$oB%uB$oB#iB$oB$oB#jB#jB#jB(�B(�B+�B/�B/�B0�B1�B3�B6�B7�B8�B9�B@B?B>	BABC(BG@BHFBKYBMeBPxBU�BW�BZ�B\�B]�Ba�Bc�Bf�BhBiBjBjBjBjBn)Bn)Bo/Bp5BrABsGBtNBvZB{yB~�B�B��B��B��B��B��B��B�B�B�B�0B�<B�`B�lB�rB�yB�rB�rB�yB��B��B��B��B��B��B��B��B�B�B�*B�*B�5B�;B�HB�HB�HB�ZB�sBӅBՑBՑBՑBՑBՑBأB��B��B��B��B�B�B�HB�kB��B	 �B	�B	�B	�B	�B	�B	�B	�B	�B	B	B	B	.B	:B	"YB	+�B	1�B	7�B	8�B	8�B	8�B	:�B	<�B	AB	AB	AB	CB	E(B	H:B	LRB	OdB	S}B	V�B	Y�B	Z�B	Y�B	Z�B	]�B	^�B	c�B	c�B	c�B	kB	s9B	vLB	zdB	~|B	~|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�,B	�2B	�2B	�EB	�QB	�WB	�cB	�cB	�pB	�vB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	� B	�B	� B	�&B	�2B	�?B	�KB	�KB	�QB	�QB	�]B	�oB	�uB	�{B	ՁB	ՁB	ևB	׎B	׎B	ؔB	ٚB	ڠB	ڠB	ڠB	ۦB	ܬB	ܬB	ܬB	ܬB	޹B	߿B	��B	��B	��B	��B	��B	��B	��G�O�B	��B	�QB
 �B

VB
1B
9B
'�B
-@B
4jB
9TB
>�B
D/B
JTB
MfB
T�B
X�B
^0B
c�B
e�B
i�B
n)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.22 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144112022020411441120220204114411  AO  ARCAADJP                                                                    20200619170854    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170854  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170854  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114411  IP                  G�O�G�O�G�O�                