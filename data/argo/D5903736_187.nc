CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-06-20T07:01:18Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20170620070118  20190604094028  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�z?V4j1   @�{t��@4vȴ9X�d�r� Ĝ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dhy�Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy~�D�qD�L�D�z=D���D�HD�?�D��D��RD��D�C�D�yHD�ڏD��D�?\D�|{D��qD�HD�U�D�y�D��q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @~�Q@���@���Az�A<z�A\z�A|z�A�=qA�=qA�=qA�=qA�
>A�=qA�=qA�=qB�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��\B��\B��\B��\B��\B��\B��\B��\B��\B�B��\B��\B��\B��\B��\B��\BÏ\BǏ\Bˏ\BϏ\Bӏ\B׏\Bۏ\Bߏ\B�\B�\B�\B�\B�\B��\B��\B��\CǮCǮCǮCǮC	ǮCǮCǮCǮCǮCǮCǮCǮCǮCǮCǮCǮC!ǮC#ǮC%ǮC'ǮC)ǮC+ǮC-ǮC/ǮC1ǮC3ǮC5ǮC7ǮC9ǮC;ǮC=ǮC?ǮCAǮCCǮCEǮCGǮCIǮCKǮCMǮCOǮCQǮCSǮCUǮCWǮCYǮC[ǮC]ǮC_ǮCaǮCcǮCeǮCgǮCiǮCkǮCmǮCoǮCqǮCsǮCuǮCwǮCyǮC{ǮC}ǮCǮC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D q�D ��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��D	q�D	��D
q�D
��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��D q�D ��D!q�D!��D"q�D"��D#q�D#��D$q�D$��D%q�D%��D&q�D&��D'q�D'��D(q�D(��D)q�D)��D*q�D*��D+q�D+��D,q�D,��D-q�D-��D.q�D.��D/q�D/��D0q�D0��D1q�D1��D2q�D2��D3q�D3��D4q�D4��D5q�D5��D6q�D6��D7q�D7��D8q�D8��D9q�D9��D:q�D:��D;q�D;��D<q�D<��D=q�D=��D>q�D>��D?q�D?��D@q�D@��DAq�DA��DBq�DB��DCq�DC��DDq�DD��DEq�DE��DFq�DF��DGq�DG��DHq�DH��DIq�DI��DJq�DJ��DKq�DK��DLq�DL��DMq�DM��DNq�DN��DOq�DO��DPq�DP��DQq�DQ��DRq�DR��DSq�DS��DTq�DT��DUq�DU��DVq�DV��DWq�DW��DXq�DX��DYq�DY��DZq�DZ��D[q�D[��D\q�D\��D]q�D]��D^q�D^��D_q�D_��D`q�D`��Daq�Da��Dbq�Db��Dcq�Dc��Ddq�Dd��Deq�De��Dfq�Df��Dgq�Dg��Dhk�Dh��Diq�Di��Djq�Dj��Dkq�Dk��Dlq�Dl��Dmq�Dm��Dnq�Dn��Doq�Do��Dpq�Dp��Dqq�Dq��Drq�Dr��Dsq�Ds��DtXRDyp�D�gD�E�D�s3D���D��>D�8�D�~D��HD��D�<�D�r>D�ӅD���D�8RD�uqD�gD�>D�N�D�r�D��g11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�  Aک�AڍPA�M�A��A��yA�G�A�&�A�l�AԶFAԑhA�l�A�;dA��A��yAң�A�A�K�AͼjA�Q�A�r�A��A� �Aȥ�A��;A��;A�M�A�~�A�JA®A�{A���A���A��A��A�jA��A�dZA���A�XA�A�A�oA��A�1A�p�A���A�p�A�z�A��A�S�A��A�hsA�&�A�`BA�%A��A�oA�%A�O�A���A�E�A���A�&�A�5?A��
A�ȴA��^A�ZA���A�9XA�+A�^5A��HA�p�A�5?A��A���A�v�A���A���A�=qA��A�&�A��A�5?A�|�A��A���A�%A���A�$�A��A��
A�+A���A��uA��A�ZA�l�A���A&�Az1'Ax�yAxZAx$�AuXArz�AqƨAqK�Aq;dAp�Ao��AnI�Aj��Ah��Af�HAe�AeVAc&�Ab-A`�uA^JA[�A[��AZ�AY+AW�
AWAV1'AU��ATbNAR~�AP~�AO�AN-AM��ALM�AK+AI��AF�9AA\)A?�A?A>v�A>Q�A>{A;p�A:  A9;dA8-A6-A3�A1�A133A0�9A/�7A.z�A+G�A)?}A'�TA'C�A&ĜA%��A$ȴA#�PA"v�A!�TA!C�A ��A��Ax�AS�A&�A��A-A�FA�DA9XA��A\)AQ�A��AhsA��A(�AVA�hAĜA��A?}A�jAE�A��A�A;dA�A�A�^AA	��A�DA/A��A9XA��A�A&�A��A�A�mA ĜA Z@��
@�~�@��@�n�@���@��u@��\@���@���@�V@�(�@��@�bN@��y@���@�"�@��#@�Q�@ߥ�@��@�r�@ܣ�@��@�A�@��m@ם�@���@�ȴ@ְ!@��@֧�@և+@�"�@�M�@��`@�ȴ@��@���@�V@�J@͙�@�Ĝ@�bN@��@�^5@�`B@ȣ�@�9X@�33@�{@Ł@�`B@���@å�@�@�-@�x�@��;@�M�@��/@��D@��D@�9X@���@���@��h@���@��w@��@���@�V@�I�@�33@���@�n�@��u@��-@�hs@�X@�X@��@�bN@��F@�l�@���@���@�ȴ@���@��y@��@��@��@��@�^5@�@��#@�x�@��@���@��/@��u@�1'@��;@���@�\)@�K�@�C�@�C�@�K�@���@�{@���@��@��@��-@�p�@�G�@�/@���@��u@�Q�@�  @�A�@���@��m@�+@���@�~�@��\@�^5@���@���@�x�@�O�@��@�z�@�A�@��@�  @��P@�;d@���@���@�~�@�^5@�5?@�J@��-@�%@���@�(�@���@���@�|�@�l�@�dZ@�dZ@��@�~�@�$�@�{@�@���@��h@�x�@�`B@�&�@���@�I�@�  @��@���@�|�@�|�@�|�@�|�@�dZ@�33@�
=@���@�ȴ@��!@���@�v�@���@�@��h@��@�?}@��@�/@�G�@�&�@���@���@��D@�bN@�Z@�Q�@�Q�@�A�@� �@�b@���@��@��P@�l�@�
=@�v�@��#@���@��j@���@��j@��@��@���@�A�@�  @��
@��w@���@�|�@�t�@�\)@�C�@���@���@�~�@�n�@�E�@��#@�x�@�hs@�X@�?}@�/@��`@���@��u@��u@�Z@�  @���@��@�l�@�
=@��R@���@���@���@��+@�v�@�^5@�5?@�5?@�$�@�@���@��@���@��^@���@���@���@�`B@���@���@��@��D@�I�@��;@���@�t�@�;d@�
=@�E�@�:�@v��@q��@h�`@aq@W��@QY�@I��@BJ�@: �@3Mj@-@(��@$�@R�@PH@xl@8�@\)@(�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�  Aک�AڍPA�M�A��A��yA�G�A�&�A�l�AԶFAԑhA�l�A�;dA��A��yAң�A�A�K�AͼjA�Q�A�r�A��A� �Aȥ�A��;A��;A�M�A�~�A�JA®A�{A���A���A��A��A�jA��A�dZA���A�XA�A�A�oA��A�1A�p�A���A�p�A�z�A��A�S�A��A�hsA�&�A�`BA�%A��A�oA�%A�O�A���A�E�A���A�&�A�5?A��
A�ȴA��^A�ZA���A�9XA�+A�^5A��HA�p�A�5?A��A���A�v�A���A���A�=qA��A�&�A��A�5?A�|�A��A���A�%A���A�$�A��A��
A�+A���A��uA��A�ZA�l�A���A&�Az1'Ax�yAxZAx$�AuXArz�AqƨAqK�Aq;dAp�Ao��AnI�Aj��Ah��Af�HAe�AeVAc&�Ab-A`�uA^JA[�A[��AZ�AY+AW�
AWAV1'AU��ATbNAR~�AP~�AO�AN-AM��ALM�AK+AI��AF�9AA\)A?�A?A>v�A>Q�A>{A;p�A:  A9;dA8-A6-A3�A1�A133A0�9A/�7A.z�A+G�A)?}A'�TA'C�A&ĜA%��A$ȴA#�PA"v�A!�TA!C�A ��A��Ax�AS�A&�A��A-A�FA�DA9XA��A\)AQ�A��AhsA��A(�AVA�hAĜA��A?}A�jAE�A��A�A;dA�A�A�^AA	��A�DA/A��A9XA��A�A&�A��A�A�mA ĜA Z@��
@�~�@��@�n�@���@��u@��\@���@���@�V@�(�@��@�bN@��y@���@�"�@��#@�Q�@ߥ�@��@�r�@ܣ�@��@�A�@��m@ם�@���@�ȴ@ְ!@��@֧�@և+@�"�@�M�@��`@�ȴ@��@���@�V@�J@͙�@�Ĝ@�bN@��@�^5@�`B@ȣ�@�9X@�33@�{@Ł@�`B@���@å�@�@�-@�x�@��;@�M�@��/@��D@��D@�9X@���@���@��h@���@��w@��@���@�V@�I�@�33@���@�n�@��u@��-@�hs@�X@�X@��@�bN@��F@�l�@���@���@�ȴ@���@��y@��@��@��@��@�^5@�@��#@�x�@��@���@��/@��u@�1'@��;@���@�\)@�K�@�C�@�C�@�K�@���@�{@���@��@��@��-@�p�@�G�@�/@���@��u@�Q�@�  @�A�@���@��m@�+@���@�~�@��\@�^5@���@���@�x�@�O�@��@�z�@�A�@��@�  @��P@�;d@���@���@�~�@�^5@�5?@�J@��-@�%@���@�(�@���@���@�|�@�l�@�dZ@�dZ@��@�~�@�$�@�{@�@���@��h@�x�@�`B@�&�@���@�I�@�  @��@���@�|�@�|�@�|�@�|�@�dZ@�33@�
=@���@�ȴ@��!@���@�v�@���@�@��h@��@�?}@��@�/@�G�@�&�@���@���@��D@�bN@�Z@�Q�@�Q�@�A�@� �@�b@���@��@��P@�l�@�
=@�v�@��#@���@��j@���@��j@��@��@���@�A�@�  @��
@��w@���@�|�@�t�@�\)@�C�@���@���@�~�@�n�@�E�@��#@�x�@�hs@�X@�?}@�/@��`@���@��u@��u@�Z@�  @���@��@�l�@�
=@��R@���@���@���@��+@�v�@�^5@�5?@�5?@�$�@�@���@��@���@��^@���@���@���@�`B@���@���@��@��D@�I�@��;@���@�t�@�;dG�O�@�E�@�:�@v��@q��@h�`@aq@W��@QY�@I��@BJ�@: �@3Mj@-@(��@$�@R�@PH@xl@8�@\)@(�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�
B�
B�B�`B��BPBJB��B�yB�B�B��B��B��B�BB!�B;dBN�BVBdZBl�Br�B~�B�JB��B��B��B��B�B�3B�jB�}B��B��BBĜB��B��B��B��B��B��BĜB�wB�dB�FB�?B�9B�3B�9B�-B�-B�!B��B��B��B�JB�B� B}�By�Bt�Bs�BgmBYBF�B5?B/B%�B�B�B�BoBDB+B1BB��B�BǮB�jB�B�{Bu�BZB:^B"�BB
��B
�B
�ZB
��B
��B
�JB
u�B
u�B
z�B
� B
gmB
J�B
F�B
D�B
C�B
33B
 �B
�B
�B
�B
�B
�B
DB	�B	�TB	�
B	��B	ÖB	�9B	�B	��B	�oB	�B	�B	~�B	v�B	y�B	z�B	u�B	q�B	hsB	\)B	Q�B	L�B	G�B	C�B	:^B	2-B	&�B	�B��B�B�B�B�B�B�yB�`B�TB�;B�B��B��BȴBŢB��B�dB�'B�B��B��B��B��B��B��B��B��B��B��B�{B�oB�oB�hB�bB�VB�JB�=B�7B�+B�B�B� B}�B{�Bz�Bw�Bt�Bq�Bo�Bm�Bl�Bk�Bk�BiyBhsBffB`BB\)BZBW
BT�BT�BXBXBW
BW
BW
BYBYBVBT�BT�BT�BR�BXB_;B[#BR�BQ�BVBVBW
BW
BS�BR�BP�BN�BQ�B]/B\)BVBS�BR�BW
B^5B^5B]/B^5B^5B^5B_;BbNBcTBe`Bq�Bw�Bw�Bx�Bz�B}�B~�B�B�B�1B�PB�VB�uB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�XB�wB�}BÖB��B��B��B��B��B�B�#B�BB�NB�ZB�ZB�fB�mB�sB�yB�B�B��B��B	B	DB	PB	PB	VB	hB	�B	�B	&�B	0!B	2-B	33B	6FB	8RB	9XB	:^B	=qB	@�B	C�B	E�B	I�B	J�B	J�B	K�B	L�B	O�B	S�B	XB	ZB	\)B	]/B	^5B	^5B	^5B	_;B	`BB	cTB	iyB	o�B	p�B	r�B	s�B	t�B	v�B	x�B	y�B	z�B	y�B	y�B	z�B	|�B	~�B	� B	�B	�B	�+B	�DB	�VB	�\B	�bB	�hB	�uB	�{B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�9B	�LB	�RB	�RB	�XB	�^B	�dB	�dB	�jB	�}B	B	ŢB	ŢB	ǮB	ȴB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�#B	�5B	�;B	�HB	�TB	�ZB	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
B
%B
%B
%B
%B
%B
%B
%B
%B
%B
%B
%B
%B
+B
+B
1B
	7B

=B
DB
JB
JB
JB
VB
2B
�B
&�B
+B
49B
;dB
C{B
HKB
N<B
T�B
YB
_VB
cTB
f�B
j�B
q�B
x�B
}�B
� B
�3B
�E11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B��B��B��B��B�4B�B BB�B�OB�dB�vB�B�B��B�B��B�B26BE�BL�B[*BcYBiBu�B�B�lB��B��B��B��B�B�;B�LB�TB�XB�\B�hBũBŦBĠBğBƮBƱB�iB�KB�-B�B�B�B�B�B��B� B��B��B��B�WB� Bx�Bv�Bt�Bp�Bk�Bj�B^EBO�B=B,B%�B�B�B�BfB	NB$B�B�B��B��B��B��B�NB��B�aBl�BQB1LB�B
�B
��B
�B
�SB
��B
��B
�MB
l�B
l�B
q�B
v�B
^mB
A�B
=�B
;�B
:�B
*<B
�B
�B
�B
�B
�B
�B
SB	�B	�eB	�B	��B	��B	�MB	�+B	��B	��B	z*B	y&B	vB	m�B	p�B	q�B	l�B	h�B	_�B	SEB	I
B	C�B	>�B	:�B	1zB	)OB	B	�B�B��B��B��B��B�B�B܈B�|B�fB�)B��B��B��B��B��B��B�RB�5B�%B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�|B�oB�kB~]BzFBx<Bw1Bu+BsBrBoBk�Bh�Bf�Bd�Bc�Bb�Bb�B`�B_�B]�BWxBS_BQWBNDBL6BL9BOKBOIBNABNBBNFBPMBPRBM?BL8BL7BL:BJ.BOJBVvBR^BJ+BI%BM@BM?BNEBNBBK3BJ.BH BFBI'BTjBSfBM@BK4BJ2BNEBUpBUqBTmBUtBUsBUnBVvBY�BZ�B\�Bh�Bo	Bo	BpBrBu2Bv3ByEBzOBjB��B��B��B��B��B��B��B��B��B��B��B�B�B�%B�<B�GB�DB�JB�]B��B��B��B��B��B� B�B�B�0B�LB�\B�{BهBۏBەBݙBޣBߩB�B��B��B��B�B�GB	|B	�B	�B	�B	�B	�B	�B	B	'UB	)bB	*eB	-{B	/�B	0�B	1�B	4�B	7�B	:�B	<�B	@�B	A�B	A�B	B�B	C�B	GB	K'B	O?B	QLB	SYB	T^B	UgB	UgB	UcB	VjB	WsB	Z�B	`�B	f�B	g�B	i�B	j�B	k�B	m�B	pB	q	B	rB	qB	qB	rB	tB	v*B	w.B	y=B	z=B	~[B	�qB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�0B	�7B	�@B	�@B	�@B	�GB	�bB	�qB	�B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�!B	�$B	�4B	�1B	�=B	�LB	�]B	�bB	�oB	�{B	ہB	ݎB	ޒB	ޒB	ߘB	ߖB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	� B	�B	�!B	� B	�!B	�"B	�$B	�-B	�5B	�8B	�8B	�@B	�EB	�DB	�DB	�KB	�IB	�IB	�OB	�CB	�GB	�GB	�HB	�IB	�GB	�NB	�JB	�IB	�MB	�MB	�HB	�MB	�RB	�PB	�TB
 \B
eB
lB
nB
oB
oG�O�B
WB
�B
�B
"AB
+_B
2�B
:�B
?nB
EaB
K�B
P�B
V}B
ZvB
]�B
a�B
iB
o�B
uB
xBB
{YB
~i11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.22 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0), vertically averaged dS =-0.009(+/-0.001) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940282019060409402820190604094028  AO  ARCAADJP                                                                    20170620070118    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170620070118  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170620070118  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094028  IP                  G�O�G�O�G�O�                