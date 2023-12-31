CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:35Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140835  20181024140835  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @���ѻ�1   @���\�6�@5���Q��c��
=p�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�33A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C��C�  C��C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  Dy�D��D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&fD&� D'  D'� D(  D(� D)  D)�fD*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0y�D1  D1� D2  D2� D3  D3� D3��D4y�D5  D5� D6  D6� D7  D7�fD8  D8� D9  D9� D:  D:y�D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@�fDA  DA� DBfDB� DC  DC� DD  DD� DE  DEy�DF  DF� DF��DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DMy�DM��DNy�DO  DO� DP  DP�fDQfDQ� DR  DR� DR��DS� DT  DT� DU  DU� DU��DV� DW  DW� DX  DX� DX��DYy�DZ  DZ� D[  D[y�D\  D\� D]  D]y�D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj�fDk  Dk� Dl  Dly�Dl��Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw� Dy��D�W\D�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@ǮA�
A#�
AC�
Ac�
A��A��A��A��A��A��A��A��B ��B��B��B��B ��B)\)B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B��B�z�B�z�B�z�B�z�B�z�B�z�B�z�BܮB�B�z�B�z�B�z�B�z�B�z�B�z�B�z�C =qC=qC=qC=qC=qC
=qC=qC=qC=qC=qC=qC=qC=qC=qC=qC=qC =qC"=qC$=qC&=qC(=qC*=qC,=qC.=qC0=qC2=qC4=qC6=qC8=qC:WC<=qC>=qC@=qCB=qCD=qCF=qCH=qCJ=qCL=qCN=qCP=qCR=qCT=qCV=qCX=qCZ=qC\#�C^=qC`=qCb=qCd=qCf=qCh=qCj=qCl=qCn=qCp=qCr=qCt=qCv=qCx=qCz=qC|=qC~=qC��C��C��C��C�+�C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C�+�C��C�+�C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D��D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D��D\D�\D\D�\D\D�\D\D�\D\D�\D\D��D�D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&�D&�\D'\D'�\D(\D(�\D)\D)��D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0��D1\D1�\D2\D2�\D3\D3�\D4�D4��D5\D5�\D6\D6�\D7\D7��D8\D8�\D9\D9�\D:\D:��D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@��DA\DA�\DB�DB�\DC\DC�\DD\DD�\DE\DE��DF\DF�\DG�DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM��DN�DN��DO\DO�\DP\DP��DQ�DQ�\DR\DR�\DS�DS�\DT\DT�\DU\DU�\DV�DV�\DW\DW�\DX\DX�\DY�DY��DZ\DZ�\D[\D[��D\\D\�\D]\D]��D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj��Dk\Dk�\Dl\Dl��Dm�Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq��Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dw�\Dy�
D�_
D�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A԰!AԲ-AԴ9AԲ-AԲ-AԶFAԼjAԺ^AԶFAԓuA�M�A��A�;dA��Aџ�Aљ�AѓuAуA�n�A�hsA�r�A�v�A�t�A�ZA�I�A�33A�+A�(�A�&�A�
=A��AЁA�bA���A˶FA�bNA���Aƙ�A�`BA�{A��A�G�A�5?A�A�|�A�l�A�ȴA�1'A�K�A�l�A��A���A�1'A���A��!A�{A�^5A���A�(�A�ZA�&�A�Q�A�?}A�ƨA�p�A�5?A�"�A��A��A�XA���A��A�VA�33A�
=A�5?A�ZA�33A��A���A���A��-A���A�I�A��A�p�A�/A�$�A��/A��+A��A�?}A��!A�1A�`BA��A��;A���A�K�A��A���A��9A��A}x�A{S�Ayl�Ax��Ax�Avz�At�ArȴAqS�Ak��Ajv�Ai�AhAfr�Ac��A`=qA^�`A^{A\��AZbAW�7AU�AUl�AT(�AR��AQ7LAO�AN��AMl�AL��AJbNAG��AE�;AB�yA@Q�A?A=\)A:r�A9l�A8ȴA8�+A7��A6bA4��A3"�A0��A0�!A0�DA/��A/C�A.A�A-�A,  A+�-A+|�A+"�A*z�A)ƨA)�A(��A(�A&r�A$�!A#�^A#O�A"~�A!`BA�mA|�A�A��A��A��A��A�!AE�AȴAJA�^AdZAK�A�`A�A|�A"�A��A�-A?}A9XA�/A`BA�A
=A	�A��A�AVA�A�
A��An�A�A�^A 5?@���@�hs@��m@�o@�v�@��@��h@�`B@�p�@���@�v�@�+@�
=@�@�7L@���@�@���@��y@�j@��@�|�@�\@�X@���@�u@�A�@㕁@���@�E�@��@��@�o@޸R@�{@�/@ܼj@��@��@�/@��@�1@�n�@���@պ^@�O�@���@��/@Լj@ԛ�@�  @��@���@���@�%@�Ĝ@ϝ�@��@���@̋D@̃@�Z@�1'@��@�1@��@�ƨ@˾w@˶F@˕�@˕�@˅@�o@�^5@�{@���@�?}@��`@�Ĝ@Ȭ@�j@�b@Ǖ�@ǍP@�l�@��@�V@�/@ă@Å@�E�@�p�@�j@��;@�ƨ@��w@��P@�@�V@��7@�%@��w@��H@��\@���@��9@�I�@�9X@��@�\)@���@�E�@��T@���@���@��7@��@���@�j@��u@�r�@�1'@�t�@��y@���@���@�ȴ@�ȴ@���@��R@���@��\@�~�@�v�@�~�@�J@�@��T@���@��7@�p�@�`B@�`B@�`B@�hs@�`B@�7L@��u@�1@�33@�7L@�1@���@��+@�`B@���@�Ĝ@��9@��u@��@��D@��D@��@�bN@�Z@�I�@� �@��m@���@�l�@�;d@���@��@���@��!@�5?@�p�@�7L@���@�Ĝ@�Q�@��@��@�o@��!@�~�@�ff@�V@�{@��@��@��@� �@�Z@���@��D@�I�@��
@���@��P@�t�@�K�@�"�@��@��@�ȴ@���@�n�@�^5@�=q@��@��-@��7@�7L@��`@��u@��@��@�;d@��y@��!@���@�^5@��@��@��^@���@��@��`@���@��/@��@���@��@��`@��`@���@� �@�+@���@�5?@�=q@�=q@�=q@��@���@��7@��@�`B@���@��j@��@��@��@���@�r�@�bN@�Z@�I�@�1'@�(�@�(�@�1'@�1'@��@�S�@���@�M�@�-@��@�J@���@���@�x�@��@���@�%@���@�/@���@��h@�7L@�1'@�  @��
@�ƨ@�ƨ@��w@�`B@z�!@g@O111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A԰!AԲ-AԴ9AԲ-AԲ-AԶFAԼjAԺ^AԶFAԓuA�M�A��A�;dA��Aџ�Aљ�AѓuAуA�n�A�hsA�r�A�v�A�t�A�ZA�I�A�33A�+A�(�A�&�A�
=A��AЁA�bA���A˶FA�bNA���Aƙ�A�`BA�{A��A�G�A�5?A�A�|�A�l�A�ȴA�1'A�K�A�l�A��A���A�1'A���A��!A�{A�^5A���A�(�A�ZA�&�A�Q�A�?}A�ƨA�p�A�5?A�"�A��A��A�XA���A��A�VA�33A�
=A�5?A�ZA�33A��A���A���A��-A���A�I�A��A�p�A�/A�$�A��/A��+A��A�?}A��!A�1A�`BA��A��;A���A�K�A��A���A��9A��A}x�A{S�Ayl�Ax��Ax�Avz�At�ArȴAqS�Ak��Ajv�Ai�AhAfr�Ac��A`=qA^�`A^{A\��AZbAW�7AU�AUl�AT(�AR��AQ7LAO�AN��AMl�AL��AJbNAG��AE�;AB�yA@Q�A?A=\)A:r�A9l�A8ȴA8�+A7��A6bA4��A3"�A0��A0�!A0�DA/��A/C�A.A�A-�A,  A+�-A+|�A+"�A*z�A)ƨA)�A(��A(�A&r�A$�!A#�^A#O�A"~�A!`BA�mA|�A�A��A��A��A��A�!AE�AȴAJA�^AdZAK�A�`A�A|�A"�A��A�-A?}A9XA�/A`BA�A
=A	�A��A�AVA�A�
A��An�A�A�^A 5?@���@�hs@��m@�o@�v�@��@��h@�`B@�p�@���@�v�@�+@�
=@�@�7L@���@�@���@��y@�j@��@�|�@�\@�X@���@�u@�A�@㕁@���@�E�@��@��@�o@޸R@�{@�/@ܼj@��@��@�/@��@�1@�n�@���@պ^@�O�@���@��/@Լj@ԛ�@�  @��@���@���@�%@�Ĝ@ϝ�@��@���@̋D@̃@�Z@�1'@��@�1@��@�ƨ@˾w@˶F@˕�@˕�@˅@�o@�^5@�{@���@�?}@��`@�Ĝ@Ȭ@�j@�b@Ǖ�@ǍP@�l�@��@�V@�/@ă@Å@�E�@�p�@�j@��;@�ƨ@��w@��P@�@�V@��7@�%@��w@��H@��\@���@��9@�I�@�9X@��@�\)@���@�E�@��T@���@���@��7@��@���@�j@��u@�r�@�1'@�t�@��y@���@���@�ȴ@�ȴ@���@��R@���@��\@�~�@�v�@�~�@�J@�@��T@���@��7@�p�@�`B@�`B@�`B@�hs@�`B@�7L@��u@�1@�33@�7L@�1@���@��+@�`B@���@�Ĝ@��9@��u@��@��D@��D@��@�bN@�Z@�I�@� �@��m@���@�l�@�;d@���@��@���@��!@�5?@�p�@�7L@���@�Ĝ@�Q�@��@��@�o@��!@�~�@�ff@�V@�{@��@��@��@� �@�Z@���@��D@�I�@��
@���@��P@�t�@�K�@�"�@��@��@�ȴ@���@�n�@�^5@�=q@��@��-@��7@�7L@��`@��u@��@��@�;d@��y@��!@���@�^5@��@��@��^@���@��@��`@���@��/@��@���@��@��`@��`@���@� �@�+@���@�5?@�=q@�=q@�=q@��@���@��7@��@�`B@���@��j@��@��@��@���@�r�@�bN@�Z@�I�@�1'@�(�@�(�@�1'@�1'@��@�S�@���@�M�@�-@��@�J@���@���@�x�@��@���@�%@���@�/@���@��h@�7L@�1'@�  @��
@�ƨ@�ƨ@��w@�`B@z�!@g@O111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�jB
�jB
�jB
�jB
�qB
��B
ĜB
��B
�#B
��B�B?}B�B�B�B �B%�B%�B"�B"�B&�B(�B(�B,B+B+B+B+B+B+B)�B'�B#�B�B�B �B$�B$�B%�B(�B33B:^B=qBA�BR�BVBW
BYB[#B\)B\)B\)B\)BYBVBVBVBR�BH�BF�BG�BE�BA�B?}B=qB=qB<jB9XB1'B�BB�B�;B��B�}B��Bz�Bw�Bt�BaHB=qB.B�BB
�HB
ǮB
B
��B
�dB
�3B
��B
��B
�bB
�B
ZB
B�B
:^B
7LB
1'B
$�B
�B
+B
B	�sB	�B	��B	ȴB	ÖB	�^B	�B	��B	�{B	w�B	r�B	k�B	dZB	\)B	N�B	=qB	33B	-B	$�B	�B	
=B	B��B��B�B�B�sB�fB�NB�;B�B��B��BǮBĜBĜB��BŢB�wB�^B�RB�3B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�\B�VB�=B�7B�B{�B|�B~�B~�B|�By�Bx�Bz�By�Bw�Bu�Bq�Bn�Bl�Bk�BiyBhsBgmBdZB_;BbNBe`BffBiyBk�BjBiyBs�B}�B�B�B�%B�DB�VB�bB�hB�{B��B��B��B��B��B��B��B��B��B��B�uB�hB�\B�PB�7B�PB�\B�VB�JB�DB�=B�7B�7B�\B��B��B��B�B�!B�qBBĜBĜBƨBȴBɺB��B��B��B��B��B��B��B��B��B�B�)B�/B�BB�NB�TB�TB�`B�fB�yB�yB�yB�B�B�B��B��B	B	B	
=B	VB	VB	VB	\B	hB	{B	�B	�B	�B	"�B	#�B	&�B	)�B	)�B	)�B	(�B	+B	-B	1'B	6FB	<jB	>wB	>wB	A�B	B�B	G�B	O�B	Q�B	R�B	R�B	R�B	S�B	S�B	T�B	VB	W
B	YB	[#B	[#B	\)B	`BB	bNB	hsB	iyB	iyB	jB	k�B	k�B	k�B	k�B	k�B	l�B	m�B	n�B	o�B	u�B	u�B	z�B	{�B	{�B	x�B	w�B	y�B	z�B	z�B	{�B	|�B	}�B	}�B	}�B	~�B	~�B	� B	�B	�B	�B	�B	�%B	�%B	�+B	�+B	�+B	�=B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�-B	�FB	�LB	�LB	�RB	�RB	�RB	�XB	�XB	�^B	�^B	�dB	�dB	�dB	�jB	�qB	�qB	�wB	�}B	��B	B	ÖB	ŢB	ǮB	ȴB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�/B	�5B	�5B	�BB	�BB	�BB	�NB	�TB	�ZB	�`B	�`B	�`B	�`B	�`B	�mB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
%�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�jB
�jB
�jB
�jB
�qB
��B
ĜB
��B
�#B
��B�B?}B�B�B�B �B%�B%�B"�B"�B&�B(�B(�B,B+B+B+B+B+B+B)�B'�B#�B�B�B �B$�B$�B%�B(�B33B:^B=qBA�BR�BVBW
BYB[#B\)B\)B\)B\)BYBVBVBVBR�BH�BF�BG�BE�BA�B?}B=qB=qB<jB9XB1'B�BB�B�;B��B�}B��Bz�Bw�Bt�BaHB=qB.B�BB
�HB
ǮB
B
��B
�dB
�3B
��B
��B
�bB
�B
ZB
B�B
:^B
7LB
1'B
$�B
�B
+B
B	�sB	�B	��B	ȴB	ÖB	�^B	�B	��B	�{B	w�B	r�B	k�B	dZB	\)B	N�B	=qB	33B	-B	$�B	�B	
=B	B��B��B�B�B�sB�fB�NB�;B�B��B��BǮBĜBĜB��BŢB�wB�^B�RB�3B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�\B�VB�=B�7B�B{�B|�B~�B~�B|�By�Bx�Bz�By�Bw�Bu�Bq�Bn�Bl�Bk�BiyBhsBgmBdZB_;BbNBe`BffBiyBk�BjBiyBs�B}�B�B�B�%B�DB�VB�bB�hB�{B��B��B��B��B��B��B��B��B��B��B�uB�hB�\B�PB�7B�PB�\B�VB�JB�DB�=B�7B�7B�\B��B��B��B�B�!B�qBBĜBĜBƨBȴBɺB��B��B��B��B��B��B��B��B��B�B�)B�/B�BB�NB�TB�TB�`B�fB�yB�yB�yB�B�B�B��B��B	B	B	
=B	VB	VB	VB	\B	hB	{B	�B	�B	�B	"�B	#�B	&�B	)�B	)�B	)�B	(�B	+B	-B	1'B	6FB	<jB	>wB	>wB	A�B	B�B	G�B	O�B	Q�B	R�B	R�B	R�B	S�B	S�B	T�B	VB	W
B	YB	[#B	[#B	\)B	`BB	bNB	hsB	iyB	iyB	jB	k�B	k�B	k�B	k�B	k�B	l�B	m�B	n�B	o�B	u�B	u�B	z�B	{�B	{�B	x�B	w�B	y�B	z�B	z�B	{�B	|�B	}�B	}�B	}�B	~�B	~�B	� B	�B	�B	�B	�B	�%B	�%B	�+B	�+B	�+B	�=B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�-B	�FB	�LB	�LB	�RB	�RB	�RB	�XB	�XB	�^B	�^B	�dB	�dB	�dB	�jB	�qB	�qB	�wB	�}B	��B	B	ÖB	ŢB	ǮB	ȴB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�/B	�5B	�5B	�BB	�BB	�BB	�NB	�TB	�ZB	�`B	�`B	�`B	�`B	�`B	�mB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
%�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.24 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140835                              AO  ARCAADJP                                                                    20181024140835    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140835  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140835  QCF$                G�O�G�O�G�O�0               