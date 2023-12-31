CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-11-11T08:01:27Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20171111080127  20190604094030  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�4m	��1   @�4�r(@5i�^5?}�d�z�G�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�ffA�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBv��B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�RD�fD�PRD���D��)D�=D�P�D�y�D���D��D��D�� D��D��=D�MD�{3D໅D���D�C�D�{D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @z=p@��@��A��A<��A\��A|��A�z�A��GA�z�A�z�A�z�A�z�A�z�A�z�B=qB=qB=qB=qB'=qB/=qB7=qB?=qBG=qBO=qBW=qB_=qBg=qBo��Bv
>B~�B���B���B���B���B���B���B���B���B���B���B���B���B���B�k�B���B���BÞ�BǞ�B˞�BϞ�BӞ�Bמ�B۞�Bߞ�B㞸B瞸B랸BB�B���B���B���C�\C�\C�\C�\C	�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C!�\C#�\C%�\C'�\C)�\C+�\C-�\C/�\C1�\C3�\C5�\C7�\C9�\C;�\C=�\C?�\CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO�\CQ�\CS�\CU�\CW�\CY�\C[�\C]�\C_�\Ca�\Cc�\Ce�\Cg�\Ci�\Ck�\Cm�\Co�\Cq�\Cs�\Cu�\Cw�\Cy�\C{�\C}�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D s�D ��Ds�D��DmqD��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D	s�D	��D
s�D
��Ds�D��Ds�D��Ds�D��DmqD��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D s�D ��D!s�D!��D"s�D"��D#s�D#��D$s�D$��D%s�D%��D&s�D&��D's�D'��D(s�D(��D)s�D)��D*s�D*��D+s�D+��D,s�D,��D-s�D-��D.s�D.��D/s�D/��D0s�D0��D1s�D1��D2s�D2��D3s�D3��D4s�D4��D5s�D5��D6s�D6��D7s�D7��D8s�D8��D9s�D9��D:s�D:��D;s�D;��D<s�D<��D=s�D=��D>s�D>��D?s�D?��D@s�D@��DAs�DA��DBs�DB��DCs�DC��DDs�DD��DEs�DE��DFs�DF��DGs�DG��DHs�DH��DIs�DI��DJs�DJ��DKs�DK��DLs�DL��DMs�DM��DNs�DN��DOs�DO��DPs�DP��DQs�DQ��DRs�DR��DSs�DS��DTs�DT��DUs�DU��DVs�DV��DWs�DW��DXs�DX��DYs�DY��DZs�DZ��D[s�D[��D\s�D\��D]s�D]��D^s�D^��D_s�D_��D`s�D`��Das�Da��Dbs�Db��Dcs�Dc��Dds�Dd��Des�De��Dfs�Df��Dgs�Dg��Dhs�Dh��Dis�Di��Djs�Dj��Dks�Dk��Dls�Dl��Dms�Dm��Dns�Dn��Dos�Do��Dps�Dp��Dqs�Dq��Drs�Dr��Dss�Ds��DtmqDy|)D� RD�J>D���D��D�)D�J�D�s�D���D���D��{D�y�D��D��)D�GD�uD�qD���D�=qD�gD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AܑhA�|�A�ZA�I�A�E�A�/A�+A�(�A�(�A� �A� �A� �A� �A� �A��A��A��A��A��A��A��A� �A� �A�"�A��A�$�A�$�A�"�A��A�1A�Q�A�S�A��mAˮA�ĜAȣ�A�\)A�bA���A�"�A�XA�/AöFA��A�=qA��FA�9XA��A��9A��\A�C�A���A�~�A�1'A�x�A���A��!A�ĜA�VA�jA�oA�^5A�G�A��A��DA��jA���A�VA�p�A�K�A���A��A� �A���A�bA�n�A��A�O�A��TA��A�r�A�`BA�=qA�`BA��A�K�A�;dA��
A� �A��
A���A�ffA�bNA�t�A�$�A�%A��A��^A�dZA��jA�`BA�+A��A��A�
=A��A���A�l�A��/A��hA��A��A��A��-A���A���A�G�A���A���A��
A��A�^5A�S�A�JA�A~��A}�wAw�;Ast�Arn�Aq;dAm�AkS�AjVAgt�Ad��Ac��Ac"�Aa�A^��A]O�A[��AY�AU�-ATA�AS�hAPĜANQ�AM��AM/AL��AK;dAI�;AH��AG&�AD�`ACx�AB{AAK�A@JA??}A>�A>^5A=��A<ȴA;G�A:JA8�uA7�A7�^A7hsA5�TA3`BA0�+A.�A+�7A(��A%ƨA$=qA#�PA"�9A!�A bNAS�A^5A-A�^AbNA�A��A�!A�A5?A�FA�`A��AI�A�TA�A%A�A�hA/AE�A�
A��AA�A
�HA
Q�A	/A�\Ap�A�uA5?A�
AS�A~�A
=A~�A�A �R@�n�@��#@�`B@���@��@�ƨ@�@��7@�@�$�@�1@�$�@�r�@�F@�E�@��`@�1@�o@�ff@�hs@�
=@�b@ޏ\@�?}@��`@�I�@�C�@�n�@��@���@�&�@�Q�@���@���@�G�@�b@Ӆ@�
=@�^5@�9X@�dZ@��@�n�@��T@͡�@��@��/@�t�@Ɂ@�Z@�C�@�^5@�@���@�A�@���@�o@��@��@�X@�&�@���@��`@�Ĝ@�9X@�C�@�K�@�33@�+@�+@���@�ff@�E�@�J@��#@��`@�|�@�J@���@��9@�9X@���@��;@��w@�l�@�+@��@�n�@�@�X@�V@��`@�Ĝ@�z�@�dZ@�@��!@�v�@�J@��h@�`B@�O�@���@�dZ@�
=@��@���@���@�Ĝ@�  @��w@�o@��+@�=q@�J@���@�hs@��@��u@�1'@��m@��@�C�@���@�v�@���@���@�x�@�O�@�V@�bN@��m@���@�dZ@�o@���@��R@��R@��R@��R@��R@�M�@��@��^@��-@��-@���@��h@��@�hs@�X@�?}@��@���@��j@��@�1'@��
@���@���@��P@�|�@�t�@�\)@�"�@��@��!@�n�@�V@�{@�@��@��@�%@��@��@�+@���@�E�@���@�@�hs@���@��j@���@�z�@�Q�@�(�@�(�@�b@���@�S�@��@���@���@�v�@�M�@��^@���@��j@�bN@�  @���@�t�@�+@�ȴ@���@��+@��+@�~�@�=q@�@�`B@�V@��@��j@�Z@�9X@�9X@��@��F@��P@��@���@��R@��R@���@��+@�^5@�E�@��#@��h@�p�@�`B@�`B@�7L@�V@���@�Ĝ@���@�Z@�A�@�b@�b@��@�ƨ@���@�K�@�"�@��y@�ȴ@���@�n�@�M�@�E�@�5?@���@�@���@�p�@�`B@�X@�X@�?}@���@��@��@���@~e@t�@m�@e%F@_��@V�m@O�@F��@?C�@8~@1��@*�L@%�@"�@]�@��@��@V@y�@o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   AܑhA�|�A�ZA�I�A�E�A�/A�+A�(�A�(�A� �A� �A� �A� �A� �A��A��A��A��A��A��A��A� �A� �A�"�A��A�$�A�$�A�"�A��A�1A�Q�A�S�A��mAˮA�ĜAȣ�A�\)A�bA���A�"�A�XA�/AöFA��A�=qA��FA�9XA��A��9A��\A�C�A���A�~�A�1'A�x�A���A��!A�ĜA�VA�jA�oA�^5A�G�A��A��DA��jA���A�VA�p�A�K�A���A��A� �A���A�bA�n�A��A�O�A��TA��A�r�A�`BA�=qA�`BA��A�K�A�;dA��
A� �A��
A���A�ffA�bNA�t�A�$�A�%A��A��^A�dZA��jA�`BA�+A��A��A�
=A��A���A�l�A��/A��hA��A��A��A��-A���A���A�G�A���A���A��
A��A�^5A�S�A�JA�A~��A}�wAw�;Ast�Arn�Aq;dAm�AkS�AjVAgt�Ad��Ac��Ac"�Aa�A^��A]O�A[��AY�AU�-ATA�AS�hAPĜANQ�AM��AM/AL��AK;dAI�;AH��AG&�AD�`ACx�AB{AAK�A@JA??}A>�A>^5A=��A<ȴA;G�A:JA8�uA7�A7�^A7hsA5�TA3`BA0�+A.�A+�7A(��A%ƨA$=qA#�PA"�9A!�A bNAS�A^5A-A�^AbNA�A��A�!A�A5?A�FA�`A��AI�A�TA�A%A�A�hA/AE�A�
A��AA�A
�HA
Q�A	/A�\Ap�A�uA5?A�
AS�A~�A
=A~�A�A �R@�n�@��#@�`B@���@��@�ƨ@�@��7@�@�$�@�1@�$�@�r�@�F@�E�@��`@�1@�o@�ff@�hs@�
=@�b@ޏ\@�?}@��`@�I�@�C�@�n�@��@���@�&�@�Q�@���@���@�G�@�b@Ӆ@�
=@�^5@�9X@�dZ@��@�n�@��T@͡�@��@��/@�t�@Ɂ@�Z@�C�@�^5@�@���@�A�@���@�o@��@��@�X@�&�@���@��`@�Ĝ@�9X@�C�@�K�@�33@�+@�+@���@�ff@�E�@�J@��#@��`@�|�@�J@���@��9@�9X@���@��;@��w@�l�@�+@��@�n�@�@�X@�V@��`@�Ĝ@�z�@�dZ@�@��!@�v�@�J@��h@�`B@�O�@���@�dZ@�
=@��@���@���@�Ĝ@�  @��w@�o@��+@�=q@�J@���@�hs@��@��u@�1'@��m@��@�C�@���@�v�@���@���@�x�@�O�@�V@�bN@��m@���@�dZ@�o@���@��R@��R@��R@��R@��R@�M�@��@��^@��-@��-@���@��h@��@�hs@�X@�?}@��@���@��j@��@�1'@��
@���@���@��P@�|�@�t�@�\)@�"�@��@��!@�n�@�V@�{@�@��@��@�%@��@��@�+@���@�E�@���@�@�hs@���@��j@���@�z�@�Q�@�(�@�(�@�b@���@�S�@��@���@���@�v�@�M�@��^@���@��j@�bN@�  @���@�t�@�+@�ȴ@���@��+@��+@�~�@�=q@�@�`B@�V@��@��j@�Z@�9X@�9X@��@��F@��P@��@���@��R@��R@���@��+@�^5@�E�@��#@��h@�p�@�`B@�`B@�7L@�V@���@�Ĝ@���@�Z@�A�@�b@�b@��@�ƨ@���@�K�@�"�@��y@�ȴ@���@�n�@�M�@�E�@�5?@���@�@���@�p�@�`B@�X@�X@�?}@���@��G�O�@���@~e@t�@m�@e%F@_��@V�m@O�@F��@?C�@8~@1��@*�L@%�@"�@]�@��@��@V@y�@o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBq�Bp�Bo�Bo�Bo�Bn�Bn�Bn�Bn�Bn�Bn�Bn�Bn�Bn�Bn�Bo�Bo�Bo�Bo�Bo�Bp�Bq�Bq�Br�Bt�B{�B�7B��B��BɺB��B8RBI�B-B'�B8RB6FB>wBJ�B?}B^5Bv�BT�Bs�B�hB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�JB�Bt�Bo�Bm�BhsBbNBZBVBP�BJ�BE�B@�B<jB9XB9XB8RB5?B.B"�B�B�B{BJB��B�B�TB�B��B�wB�'B��B��B�oB�DB�%B�B� B{�Bu�Br�Bl�B^5BK�BF�B@�B8RB1'B�BhB+B  B
��B
�fB
�#B
��B
��B
��B
�7B
z�B
r�B
e`B
>wB
"�B
�B
bB	��B	�yB	�BB	��B	�qB	�LB	�'B	��B	��B	�JB	~�B	k�B	[#B	R�B	L�B	B�B	9XB	6FB	33B	/B	(�B	!�B	�B	{B	JB	%B	  B��B��B�B�B�B�B�mB�NB�/B�B�B��B��BɺB�wB�'B��B��B��B�oB�\B�\B�VB�PB�JB�=B�7B�1B�%B�B�B�B~�By�Bw�Bu�Bt�Bs�Bs�Br�Bq�Bp�Bp�Bo�Bo�Bn�Bn�Bm�Bl�Bm�Bk�BhsBe`Be`BffBffBffBffBgmBjBjBjBiyBk�Bk�Bk�BjBjBn�Bo�Bo�Bs�Bv�Bv�Bw�Bx�Bx�Bz�B{�B|�B|�B{�Bz�By�B|�B|�Bz�B� B�B�B�B�B�B�B�B�B�7B�=B�VB�bB�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�!B�3B�^B�jB�wB�wB�wB��BBÖBÖBÖBŢB��B��B�B�B�)B�/B�/B�5B�BB�HB�TB�fB�yB�B�B�B�B�B��B��B��B��B	B	B	%B	%B	%B	\B	hB	oB	{B	�B	�B	#�B	%�B	+B	0!B	33B	5?B	:^B	;dB	>wB	B�B	E�B	F�B	I�B	J�B	K�B	M�B	R�B	T�B	VB	W
B	YB	]/B	`BB	aHB	cTB	dZB	ffB	ffB	ffB	ffB	ffB	ffB	hsB	jB	l�B	l�B	l�B	l�B	m�B	m�B	m�B	n�B	n�B	o�B	p�B	q�B	r�B	s�B	u�B	w�B	w�B	w�B	w�B	x�B	x�B	z�B	z�B	|�B	}�B	}�B	� B	�B	�B	�B	�B	�B	�=B	�JB	�PB	�\B	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�FB	�RB	�XB	�^B	�dB	�dB	�jB	�}B	B	ĜB	ǮB	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�#B	�/B	�5B	�BB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
�B
�B
&�B
,WB
7�B
=�B
E9B
JrB
Q B
VmB
\�B
`�B
f2B
g8B
l�B
o�B
t9B
x�B
}�B
�B
��11111111111111111111111111111111111111141141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   BeSBdQBcGBcKBcEBbFBbABbABbABbBBbDBbFBbFBbBBbBBcKBcIBcLBcIBcIBdRBeQBeQBf\BheBo�B|�B�?B��B�_B�hB+�B=QB �B�B+�B)�B2B>ZG�O�BQ�Bj_G�O�BgNB��B��B�9B�UB�WB�VB�eB�lB�nB�pB�dB�cB�qB�tB�rB�pB�ZB�:B�B�B��B�Bw�Bh\Bc;Ba2B\BU�BM�BI�BD�B>fB9DB4&B0B,�B- B+�B(�B!�B|BSBDB+B��B�B�VB�B��BB�)B��B��B�EB�(B~�By�Bw�Bs�Bo�BiBflB`HBQ�B?�B:iB4EB,B$�B�B-B
��B
��B
�B
�3B
��B
ñB
�VB
�B
}B
n�B
f�B
Y<B
2VB
�B
�B
DB	�B	�bB	�,B	��B	�\B	�;B	�B	��B	�wB	�<B	r�B	_|B	OB	F�B	@�B	6�B	-VB	*DB	'4B	#B	�B	�B	�B	~B	 JB�*B�B��B��B�B�B�BߌB�zB�WB�9B�B�B�B��B��B��B�;B�B��B��B��B�tB�qB�kB�dB�[B~QB}OB|GBz<Bx2Bw(BuBsBm�Bk�Bi�Bh�Bg�Bg�Bf�Be�Bd�Bd�Bc�Bc�Bb�Bb�Ba�B`�Ba�B_�B\�BY}BY}BZ�BZ�BZ�BZ�B[�B^�B^�B^�B]�B_�B_�B_�B^�B^�Bb�Bc�Bc�Bg�Bj�Bj�Bk�Bl�Bl�Bo Bp
BqBqBpBn�Bm�BqBqBoBtBu&Bv,Bw0Bw2Bx8By?Bx9By=B}UB~[B�wB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�"B�'B�%B�"B�B�"B�'B�/B�1B�8B�@B�SB�zB��B��B��B��B��B��B��B��B��B��B��B�	B�,B�1B�EB�JB�KB�TB�^B�`B�pB�~BݒB�B�B�B��B��B��B��B�	B�B�$B�1B�@B�>B�BB	sB	�B	�B	�B	�B	�B	�B	�B	B	$7B	'LB	)WB	.qB	/yB	2�B	6�B	9�B	:�B	=�B	>�B	?�B	A�B	GB	IB	JB	KB	M(B	QBB	TVB	UZB	WcB	XpB	ZuB	ZxB	ZxB	ZzB	ZvB	ZzB	\�B	^�B	`�B	`�B	`�B	`�B	a�B	a�B	a�B	b�B	b�B	c�B	d�B	e�B	f�B	g�B	i�B	k�B	k�B	k�B	k�B	l�B	l�B	n�B	n�B	p�B	rB	rB	tB	uB	vB	x'B	x*B	y/B	~LB	�ZB	�[B	�mB	�wB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�SB	�ZB	�aB	�iB	�nB	�pB	�sB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�B	�%B	�,B	�,B	�7B	�;B	�KB	�aB	�gB	�iB	�kB	�hB	�dB	�kB	�nB	�B	߉B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�G�O�B	��B
�B
�B
 ]B
+�B
1�B
97B
>tB
EB
JpB
P�B
T�B
Z6B
[9B
`�B
c�B
h9B
l�B
q�B
uB
t�11111111111111111111111111111111111111141141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.19 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0), vertically averaged dS =-0.012(+/-0.002) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940302019060409403020190604094030  AO  ARCAADJP                                                                    20171111080127    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20171111080127  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20171111080127  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094030  IP                  G�O�G�O�G�O�                