CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-09T00:18:46Z creation;2022-06-09T00:18:46Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \t   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `X   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ܠ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �L   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �\   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �t   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �|Argo profile    3.1 1.2 19500101000000  20220609001846  20220609040304  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               yA   JA                                  2B  A   APEX                            7906                            051216                          846 @و�d�
1   @و�����@3�I�^5�d��`A�71   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�33B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�fC  C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Ck�fCn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	fD	� D
  D
� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$fD$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9fD9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DHfDH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DSy�DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^y�D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Dh��Di� DjfDj� Dk  Dk� Dl  Dl�fDm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz�fD{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�<�D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D��3D�3D�@ D̀ D�� D���D�@ D̓3D�� D�  D�@ D΀ D�� D�  D�C3Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۃ3D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D��3D�� D�  D�@ D�3D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D���D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�l�D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�z@ǮA�
A#�
AC�
Ac�
A��A��A��A��A¸RA��A��A��B ��B	\)B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�G�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�C =qC=qC=qC=qC=qC
=qC#�C=qCWC=qC=qC=qC=qC=qC=qC=qC =qC"=qC$=qC&=qC(=qC*=qC,=qC.=qC0=qC2=qC4=qC6=qC8#�C:#�C<=qC>=qC@=qCB=qCD=qCF=qCH=qCJ=qCL=qCN=qCP=qCR=qCT=qCV=qCX=qCZ=qC\=qC^#�C`=qCb=qCd=qCf=qCh=qCj=qCl#�Cn=qCp=qCr=qCt=qCv=qCx=qCz=qC|=qC~=qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	�D	�\D
\D
�\D\D�\D�D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D��D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$�D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9�D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH�DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS��DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^��D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di�Di�\Dj�Dj�\Dk\Dk�\Dl\Dl��Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz��D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D��{D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD�{D�D{D���D�ǮD��D�G�D���D�ǮD��D�J�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�J�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D��{D��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD�
�D�G�D���D�ǮD��D�G�D���D�ǮD�{D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D��{D�ǮD��D�D{D��{D��{D��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D���D�
�D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D���D�
�D�G�D���D�ǮD��D�G�D���D�ǮD�{D�D{D���D���D��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�J�D���D�ǮD��D�G�D���D�ǮD�
�D�G�D���D�ǮD��D�G�D�D�ǮD��D�G�DÇ�D�ǮD��D�G�Dć�D�ǮD��D�G�DŇ�D�ǮD��D�G�DƇ�D�ǮD��D�G�DǇ�D�ǮD��D�G�Dȇ�D�ǮD��D�G�Dɇ�D�ǮD��D�G�Dʇ�D�ǮD��D�G�Dˇ�D���D�
�D�G�Ḋ�D�ǮD�{D�G�D͊�D�ǮD��D�G�D·�D�ǮD��D�J�Dχ�D�ǮD��D�G�DЇ�D�ǮD��D�G�Dч�D�ǮD��D�G�D҇�D�ǮD��D�G�DӇ�D�ǮD��D�G�Dԇ�D�ǮD��D�G�DՇ�D�ǮD��D�G�Dև�D�ǮD��D�G�Dׇ�D�ǮD��D�G�D؇�D�ǮD��D�G�Dه�D�ǮD��D�G�Dڇ�D�ǮD��D�G�Dۊ�D�ǮD��D�G�D܇�D�ǮD��D�G�D݇�D�ǮD��D�G�Dއ�D�ǮD��D�G�D߇�D�ǮD��D�G�D���D�ǮD��D�G�D��D�ǮD�
�D�G�D⇮D�ǮD��D�G�D㇮D�ǮD��D�D{D䇮D�ǮD��D�G�D凮D�ǮD��D�G�D懮D�ǮD�
�D�G�D燮D�ǮD��D�G�D臮D�ǮD��D�G�D釮D�ǮD��D�G�DꇮD�ǮD��D�G�D뇮D�ǮD��D�G�D쇮D�ǮD��D�G�D퇮D��{D��D�G�DD�ǮD��D�G�DD�ǮD��D�D{D��{D�ǮD��D�G�D�D�ǮD��D�G�D�D�ǮD��D�J�D�D�ǮD��D�G�D�D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D�t{D�Ǯ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��;A��/A��
Aܴ9A���AܮAܰ!Aܡ�A�t�A�Q�A�A�A�7LA�1'A�-A�-A�/A�1'A�1'A�1'A�-A�+A�&�A� �A��A��A�A��`A۴9A�ffA�E�A�
=A�x�Aٗ�A��HAדuA�A�ZA�{AӲ-Aӏ\A�{Aҗ�A�hsA�?}A�{A���A��/A��A��HA�XA���A��TA�~�A�Aɝ�A�7LA��A���A�=qA�\)Aŧ�A�A�VA�
=A�`BA�Q�A�A��\A�(�A��A���A�9XA���A��/A�A�G�A���A���A�ȴA�|�A�ZA��A�33A�O�A�ĜA�O�A�&�A�ZA�{A�&�A�C�A�C�A�bNA���A��7A��A�  A�-A�dZA���A��
A�p�A�A�?}A��yA���A�Q�A��A��A��A��A��A}XAz��Ax�\Aw�Av1'At�!Ar�jAq7LAo��Al��Ak�AjbNAg�hAdz�Abn�A`�\A^^5A\ZAY�PAVVAR(�AO�#AN��ALz�AJ�AI�AH��AG�AFAC�^A@bA=p�A:�yA9��A8 �A6~�A3�PA2�jA2ZA1�TA0n�A/��A/VA-�wA,�9A*ffA(��A(~�A'��A'"�A%�wA$�DA#C�A!�A!+A �A�#A��A��A9XA��AM�A
=A��A�;A��At�AQ�AK�AȴA�yAĜAx�A��A-A�A��A|�A�hA�\AbAXA�jA
ĜA
 �A
�9A
��A
��A
-A
 �A��A��AM�A��A�A`BA�A/A �A ��AA �jA �@�\)@�&�@��@��9@���@�J@�7L@�Ĝ@��`@��T@��@��
@��@�$�@��`@�F@�V@�%@�K�@旍@�v�@�@��`@�
=@�+@�t�@��y@�=q@�p�@�/@�n�@��@�Z@��@�$�@���@��m@�n�@ԣ�@��y@�{@�X@��@���@мj@�Q�@��y@Η�@�E�@���@͙�@�j@�t�@��#@���@Ǿw@��@��@��T@�@š�@�%@ě�@��
@��H@���@��@�t�@�^5@���@�Q�@�1'@�(�@��F@�S�@�\)@�+@���@�E�@��@��T@��-@�j@��w@���@���@��^@���@�%@��P@�
=@��R@�~�@��^@��@��@��u@�j@�1@��m@���@��
@���@��w@�dZ@��+@�=q@�E�@�&�@�b@��@�l�@��@��H@�J@�x�@�X@��@�z�@���@�&�@���@��@��@��!@�n�@�=q@��@�@��7@�G�@���@��/@��@�z�@�Q�@� �@�  @��@��
@��@���@�S�@�ff@��T@���@�@��-@���@���@�/@��9@�z�@�I�@�A�@�A�@�1@��m@��P@�S�@��@���@��y@��H@���@�^5@�-@�@��@���@�`B@���@��/@���@��@�A�@���@��@�K�@�"�@�o@��H@���@�M�@�@��#@��@�G�@��@�V@��`@���@��9@��D@�j@�1'@�  @��m@�ƨ@��@�t�@�;d@��y@�ff@���@�@���@���@��h@�G�@��`@��@�Z@��@��
@�t�@��@��H@���@�^5@��@��-@��7@��@�x�@�p�@��@��9@�z�@�A�@�  @�ƨ@���@�\)@�+@��@��@��!@��\@�ff@���@���@���@�X@��@��9@�z�@�Q�@���@���@���@�\)@�"�@�
=@��H@���@�$�@�@�@�hs@�7L@��@��`@�Ĝ@��u@�r�@�Z@�A�@��@���@��@���@�~�@�E�@�@��@���@�@�@���@��@�`B@�?}@��@�V@�%@�Ĝ@��@�I�@�b@���@��;@���@��w@�dZ@�@�ȴ@���@�ff@�=q@�$�@��@���@���@��-@���@�p�@�?}@��@���@�z�@�(�@�b@��@\)@~�y@~ff@}@}p�@}/@|�@|��@|z�@|Z@|I�@|1@{�
@{�
@{��@z��@zM�@y��@y��@y�7@x�`@x��@x1'@w�@w��@w\)@w+@w
=@vȴ@v��@v{@u�T@u��@u?}@t�/@t��@tZ@t(�@s�
@s33@r�@r��@r�\@r-@q�#@q��@q��@q7L@p��@pQ�@p �@o�;@o�@o|�@n�y@nV@n5?@m�T@l��@k�
@ko@j�!@j~�@jM�@i��@i�^@i��@i��@i��@i��@i��@i&�@h�`@h�u@h  @gl�@g+@f�@fff@e�T@e�h@e`B@e�@d�/@dz�@d1@cƨ@c��@cS�@c@b~�@a�7@aG�@a&�@a&�@a%@`�u@_�;@_;d@_�@^ȴ@]�@]@]�h@]p�@]`B@]�@\��@\�@\�D@\z�@\(�@[t�@Z�@Z�\@Z=q@Y��@Y�7@Yx�@YX@Y7L@X��@X�9@XQ�@X  @W\)@V�+@V@U�@U�@T��@T�j@TI�@S�F@S�@St�@So@R�H@R��@R=q@Q�#@QX@P�`@P�u@Pb@O�;@O��@OK�@N��@N�R@Nff@N$�@N{@M�@M��@M��@MV@L�@L�@LZ@L9X@L(�@L1@K��@KC�@K@J�\@I��@I��@Ihs@I�@H�@H  @G��@Gl�@G;d@F�y@FV@E��@E�@D��@D�j@Dz�@D1@CC�@B�@B�!@B-@Ahs@A�@@��@@��@@r�@@A�@?K�@>�y@>ȴ@>��@>ff@=�T@=O�@<�/@<Z@<1@;�
@;t�@:�H@:��@:~�@:�@9�^@9hs@8Ĝ@8Q�@81'@7��@7|�@7\)@6��@6ff@6$�@5@5��@5�h@5/@4�/@4z�@3�
@3ƨ@3�
@3�
@3�F@3��@3�@3t�@3dZ@333@3o@2�H@2�!@2n�@2J@1��@1x�@1X@17L@1�@0��@0�u@0bN@0A�@0A�@/�@/��@/|�@/K�@/�@/+@/�@.�+@.v�@.V@.$�@.@-�T@-��@-�-@-/@,��@,�@,�D@,I�@,9X@,9X@,(�@,1@+�
@+�@+S�@+"�@+@*��@*�\@*M�@*-@*�@*�@*J@)�@)��@)��@)�7@)�@(�`@(��@(��@(r�@(Q�@(A�@(b@'�w@'��@'l�@'K�@';d@'+@&�y@&ff@&{@%�T@%��@%�@%`B@%`B@%?}@%V@$�@$��@$�j@$�@$�D@$Z@$1@#��@#�m@#�
@#��@#33@#@"��@"��@"��@"n�@"=q@!�#@!��@!x�@!7L@!%@ ��@ r�@ 1'@�;@�P@l�@l�@l�@K�@+@�R@E�@�@@�h@p�@p�@O�@/@��@�@�@�D@9X@�m@�F@t�@C�@�@��@�!@�\@^5@-@J@�@�#@�^@��@�^@�^@��@��@�u@1'@  @��@�P@;d@+@
=@ȴ@v�@{@�T@�h@?}@V@�/@�@��@Z@1@�
@�F@��@33@�!@~�@~�@^5@=q@��@��@X@�`@��@Ĝ@�9@Q�@ �@ �@b@�;@��@|�@\)@�@��@�y@�y@�y@ȴ@��@V@�@�-@�@`B@/@�@��@��@�@�@�D@I�@�@��@�
@�F@��@t�@C�@"�@@
��@
^5@
M�@
M�@
=q@
=q@
-@
-@
-@	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��;A��/A��
Aܴ9A���AܮAܰ!Aܡ�A�t�A�Q�A�A�A�7LA�1'A�-A�-A�/A�1'A�1'A�1'A�-A�+A�&�A� �A��A��A�A��`A۴9A�ffA�E�A�
=A�x�Aٗ�A��HAדuA�A�ZA�{AӲ-Aӏ\A�{Aҗ�A�hsA�?}A�{A���A��/A��A��HA�XA���A��TA�~�A�Aɝ�A�7LA��A���A�=qA�\)Aŧ�A�A�VA�
=A�`BA�Q�A�A��\A�(�A��A���A�9XA���A��/A�A�G�A���A���A�ȴA�|�A�ZA��A�33A�O�A�ĜA�O�A�&�A�ZA�{A�&�A�C�A�C�A�bNA���A��7A��A�  A�-A�dZA���A��
A�p�A�A�?}A��yA���A�Q�A��A��A��A��A��A}XAz��Ax�\Aw�Av1'At�!Ar�jAq7LAo��Al��Ak�AjbNAg�hAdz�Abn�A`�\A^^5A\ZAY�PAVVAR(�AO�#AN��ALz�AJ�AI�AH��AG�AFAC�^A@bA=p�A:�yA9��A8 �A6~�A3�PA2�jA2ZA1�TA0n�A/��A/VA-�wA,�9A*ffA(��A(~�A'��A'"�A%�wA$�DA#C�A!�A!+A �A�#A��A��A9XA��AM�A
=A��A�;A��At�AQ�AK�AȴA�yAĜAx�A��A-A�A��A|�A�hA�\AbAXA�jA
ĜA
 �A
�9A
��A
��A
-A
 �A��A��AM�A��A�A`BA�A/A �A ��AA �jA �@�\)@�&�@��@��9@���@�J@�7L@�Ĝ@��`@��T@��@��
@��@�$�@��`@�F@�V@�%@�K�@旍@�v�@�@��`@�
=@�+@�t�@��y@�=q@�p�@�/@�n�@��@�Z@��@�$�@���@��m@�n�@ԣ�@��y@�{@�X@��@���@мj@�Q�@��y@Η�@�E�@���@͙�@�j@�t�@��#@���@Ǿw@��@��@��T@�@š�@�%@ě�@��
@��H@���@��@�t�@�^5@���@�Q�@�1'@�(�@��F@�S�@�\)@�+@���@�E�@��@��T@��-@�j@��w@���@���@��^@���@�%@��P@�
=@��R@�~�@��^@��@��@��u@�j@�1@��m@���@��
@���@��w@�dZ@��+@�=q@�E�@�&�@�b@��@�l�@��@��H@�J@�x�@�X@��@�z�@���@�&�@���@��@��@��!@�n�@�=q@��@�@��7@�G�@���@��/@��@�z�@�Q�@� �@�  @��@��
@��@���@�S�@�ff@��T@���@�@��-@���@���@�/@��9@�z�@�I�@�A�@�A�@�1@��m@��P@�S�@��@���@��y@��H@���@�^5@�-@�@��@���@�`B@���@��/@���@��@�A�@���@��@�K�@�"�@�o@��H@���@�M�@�@��#@��@�G�@��@�V@��`@���@��9@��D@�j@�1'@�  @��m@�ƨ@��@�t�@�;d@��y@�ff@���@�@���@���@��h@�G�@��`@��@�Z@��@��
@�t�@��@��H@���@�^5@��@��-@��7@��@�x�@�p�@��@��9@�z�@�A�@�  @�ƨ@���@�\)@�+@��@��@��!@��\@�ff@���@���@���@�X@��@��9@�z�@�Q�@���@���@���@�\)@�"�@�
=@��H@���@�$�@�@�@�hs@�7L@��@��`@�Ĝ@��u@�r�@�Z@�A�@��@���@��@���@�~�@�E�@�@��@���@�@�@���@��@�`B@�?}@��@�V@�%@�Ĝ@��@�I�@�b@���@��;@���@��w@�dZ@�@�ȴ@���@�ff@�=q@�$�@��@���@���@��-@���@�p�@�?}@��@���@�z�@�(�@�b@��@\)@~�y@~ff@}@}p�@}/@|�@|��@|z�@|Z@|I�@|1@{�
@{�
@{��@z��@zM�@y��@y��@y�7@x�`@x��@x1'@w�@w��@w\)@w+@w
=@vȴ@v��@v{@u�T@u��@u?}@t�/@t��@tZ@t(�@s�
@s33@r�@r��@r�\@r-@q�#@q��@q��@q7L@p��@pQ�@p �@o�;@o�@o|�@n�y@nV@n5?@m�T@l��@k�
@ko@j�!@j~�@jM�@i��@i�^@i��@i��@i��@i��@i��@i&�@h�`@h�u@h  @gl�@g+@f�@fff@e�T@e�h@e`B@e�@d�/@dz�@d1@cƨ@c��@cS�@c@b~�@a�7@aG�@a&�@a&�@a%@`�u@_�;@_;d@_�@^ȴ@]�@]@]�h@]p�@]`B@]�@\��@\�@\�D@\z�@\(�@[t�@Z�@Z�\@Z=q@Y��@Y�7@Yx�@YX@Y7L@X��@X�9@XQ�@X  @W\)@V�+@V@U�@U�@T��@T�j@TI�@S�F@S�@St�@So@R�H@R��@R=q@Q�#@QX@P�`@P�u@Pb@O�;@O��@OK�@N��@N�R@Nff@N$�@N{@M�@M��@M��@MV@L�@L�@LZ@L9X@L(�@L1@K��@KC�@K@J�\@I��@I��@Ihs@I�@H�@H  @G��@Gl�@G;d@F�y@FV@E��@E�@D��@D�j@Dz�@D1@CC�@B�@B�!@B-@Ahs@A�@@��@@��@@r�@@A�@?K�@>�y@>ȴ@>��@>ff@=�T@=O�@<�/@<Z@<1@;�
@;t�@:�H@:��@:~�@:�@9�^@9hs@8Ĝ@8Q�@81'@7��@7|�@7\)@6��@6ff@6$�@5@5��@5�h@5/@4�/@4z�@3�
@3ƨ@3�
@3�
@3�F@3��@3�@3t�@3dZ@333@3o@2�H@2�!@2n�@2J@1��@1x�@1X@17L@1�@0��@0�u@0bN@0A�@0A�@/�@/��@/|�@/K�@/�@/+@/�@.�+@.v�@.V@.$�@.@-�T@-��@-�-@-/@,��@,�@,�D@,I�@,9X@,9X@,(�@,1@+�
@+�@+S�@+"�@+@*��@*�\@*M�@*-@*�@*�@*J@)�@)��@)��@)�7@)�@(�`@(��@(��@(r�@(Q�@(A�@(b@'�w@'��@'l�@'K�@';d@'+@&�y@&ff@&{@%�T@%��@%�@%`B@%`B@%?}@%V@$�@$��@$�j@$�@$�D@$Z@$1@#��@#�m@#�
@#��@#33@#@"��@"��@"��@"n�@"=q@!�#@!��@!x�@!7L@!%@ ��@ r�@ 1'@�;@�P@l�@l�@l�@K�@+@�R@E�@�@@�h@p�@p�@O�@/@��@�@�@�D@9X@�m@�F@t�@C�@�@��@�!@�\@^5@-@J@�@�#@�^@��@�^@�^@��@��@�u@1'@  @��@�P@;d@+@
=@ȴ@v�@{@�T@�h@?}@V@�/@�@��@Z@1@�
@�F@��@33@�!@~�@~�@^5@=q@��@��@X@�`@��@Ĝ@�9@Q�@ �@ �@b@�;@��@|�@\)@�@��@�y@�y@�y@ȴ@��@V@�@�-@�@`B@/@�@��@��@�@�@�D@I�@�@��@�
@�F@��@t�@C�@"�@@
��@
^5@
M�@
M�@
=q@
=q@
-@
-@
-@	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
.B
.B
.B
-B
-B
-B
-B
-B
,B
+B
)�B
)�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
(�B
(�B
'�B
'�B
&�B
&�B
$�B
 �B
�B
�B
uB
VB

=B
  B	��B	�NB	��B	�dB	�B	��B	��B	�-B	�RB	�jB	�jB	�jB	�jB	�jB	ŢB	�`B	��B
	7B
33B
dZB
�hB
�XB
��B
ƨB
��B�B0!BE�B^5B~�B�hB�B��B�qB�RB��BVB1BB  B��B�B��B�\By�B{�B{�B{�Bu�Bv�Bz�Bu�BVBVBiyBt�Br�Bl�BhsB]/BS�BE�B<jB49B+B#�B�B
=BB
�B
��B
��B
�-B
��B
�B
s�B
W
B
@�B
�B
JB	��B	�B	�sB	�NB	�B	��B	B	�qB	�B	��B	��B	�oB	�+B	|�B	r�B	iyB	cTB	VB	J�B	=qB	6FB	2-B	-B	$�B	�B	�B	�B	{B	JB		7B	B	B��B�B��B�mB�`B�ZB�fB�B�B�B�B�yB�fB�HB�;B�/B�B�B��B��B��B��B��B��BɺB�B�5B�/B�B��B��BŢB�?B�-B�-B�!B�'B�?B�jB�XB�3B�3B�-B�}B�qB�}B�dB��B��BŢBÖB��B��B�B�B�B�#B�5B�TB�TB�#B�B�B�fB�)BɺB��B�B�sB�ZB��B�'B�B��B�wB�^B�XB�dBÖB��B�#B�B�B�
B�B�
B��B��B��B�B�B�B�B��B�B�B�/B�#B�B�B��B��B��B�B�B�B�B�B��B��B�
B�B�B�B�#B�/B�;B�BB�HB�NB�ZB�yB�B�B�B�B��B��B��B��B��B��B��B	B	B	%B	B	B		7B	DB	JB	JB	VB	�B	�B	!�B	#�B	&�B	)�B	+B	.B	/B	33B	5?B	7LB	>wB	?}B	?}B	?}B	@�B	B�B	B�B	B�B	E�B	J�B	P�B	Q�B	S�B	XB	ZB	cTB	gmB	iyB	l�B	o�B	q�B	q�B	t�B	u�B	t�B	t�B	{�B	�B	�B	�B	�B	�B	�B	�%B	�=B	�oB	�{B	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�?B	�LB	�RB	�dB	�jB	�jB	�jB	�qB	�}B	��B	B	B	B	ĜB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�B	�B	�#B	�#B	�)B	�/B	�/B	�5B	�;B	�BB	�BB	�BB	�BB	�HB	�HB	�NB	�TB	�ZB	�`B	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
1B
1B
	7B

=B

=B

=B
DB
JB
PB
VB
VB
\B
bB
bB
hB
hB
oB
oB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
!�B
"�B
"�B
!�B
"�B
"�B
!�B
"�B
#�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
+B
+B
+B
,B
-B
-B
-B
-B
-B
-B
.B
.B
.B
.B
.B
/B
.B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
:^B
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
;dB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
VB
W
B
W
B
W
B
W
B
XB
XB
XB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
]/B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
cTB
cTB
dZB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�01111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
.B
.B
.B
-B
-B
-B
-B
-B
,B
+B
)�B
)�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
(�B
(�B
'�B
'�B
&�B
&�B
$�B
 �B
�B
�B
uB
VB

=B
  B	��B	�NB	��B	�dB	�B	��B	��B	�-B	�RB	�jB	�jB	�jB	�jB	�jB	ŢB	�`B	��B
	7B
33B
dZB
�hB
�XB
��B
ƨB
��B�B0!BE�B^5B~�B�hB�B��B�qB�RB��BVB1BB  B��B�B��B�\By�B{�B{�B{�Bu�Bv�Bz�Bu�BVBVBiyBt�Br�Bl�BhsB]/BS�BE�B<jB49B+B#�B�B
=BB
�B
��B
��B
�-B
��B
�B
s�B
W
B
@�B
�B
JB	��B	�B	�sB	�NB	�B	��B	B	�qB	�B	��B	��B	�oB	�+B	|�B	r�B	iyB	cTB	VB	J�B	=qB	6FB	2-B	-B	$�B	�B	�B	�B	{B	JB		7B	B	B��B�B��B�mB�`B�ZB�fB�B�B�B�B�yB�fB�HB�;B�/B�B�B��B��B��B��B��B��BɺB�B�5B�/B�B��B��BŢB�?B�-B�-B�!B�'B�?B�jB�XB�3B�3B�-B�}B�qB�}B�dB��B��BŢBÖB��B��B�B�B�B�#B�5B�TB�TB�#B�B�B�fB�)BɺB��B�B�sB�ZB��B�'B�B��B�wB�^B�XB�dBÖB��B�#B�B�B�
B�B�
B��B��B��B�B�B�B�B��B�B�B�/B�#B�B�B��B��B��B�B�B�B�B�B��B��B�
B�B�B�B�#B�/B�;B�BB�HB�NB�ZB�yB�B�B�B�B��B��B��B��B��B��B��B	B	B	%B	B	B		7B	DB	JB	JB	VB	�B	�B	!�B	#�B	&�B	)�B	+B	.B	/B	33B	5?B	7LB	>wB	?}B	?}B	?}B	@�B	B�B	B�B	B�B	E�B	J�B	P�B	Q�B	S�B	XB	ZB	cTB	gmB	iyB	l�B	o�B	q�B	q�B	t�B	u�B	t�B	t�B	{�B	�B	�B	�B	�B	�B	�B	�%B	�=B	�oB	�{B	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�?B	�LB	�RB	�dB	�jB	�jB	�jB	�qB	�}B	��B	B	B	B	ĜB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�B	�B	�#B	�#B	�)B	�/B	�/B	�5B	�;B	�BB	�BB	�BB	�BB	�HB	�HB	�NB	�TB	�ZB	�`B	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
1B
1B
	7B

=B

=B

=B
DB
JB
PB
VB
VB
\B
bB
bB
hB
hB
oB
oB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
!�B
"�B
"�B
!�B
"�B
"�B
!�B
"�B
#�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
+B
+B
+B
,B
-B
-B
-B
-B
-B
-B
.B
.B
.B
.B
.B
/B
.B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
:^B
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
;dB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
VB
W
B
W
B
W
B
W
B
XB
XB
XB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
]/B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
cTB
cTB
dZB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�01111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20220609091343  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220609001846  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220609001846  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220609001846                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220609091851  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220609091851  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220609040304                      G�O�G�O�G�O�                