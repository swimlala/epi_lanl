CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:21Z creation      
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
resolution        =���   axis      Z        h  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  S`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  mP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140821  20181024140821  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               aA   AO  6560                            2B  A   APEX                            7471                            062512                          846 @���$�a�1   @��彡B<@3�1&�y�c���E�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      aA   A   A   @�ff@�  A   A   A@  A`  A�  A�33A�33A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBhffBo��Bx  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B���B�  B�  C   C  C  C  C  C
  C  C  C  C�C  C  C  C  C  C  C   C"�C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C}�fC�fC�  C�  C�  C�  C��C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��C�  C��3C��3C�  C��D fD �fD  D� D  D� D  D� D  D� D��Dy�D��D� D  Dy�D��Dy�D��D	y�D
  D
� D  Dy�D��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D  D�fDfD� D  Dy�D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)fD)� D*  D*� D+  D+� D,  D,y�D-  D-� D-��D.y�D/  D/� D0  D0� D1  D1y�D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DEfDE� DF  DF� DG  DG� DH  DH� DI  DIy�DJ  DJ� DK  DK� DL  DL� DM  DM� DM��DN� DO  DO� DP  DP�fD\�fD]fD]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df�fDgfDg� Dh  Dh� DifDi�fDj  Dj� Dk  Dk� Dk��Dly�Dm  Dm� Dn  Dn� Do  Do�fDp  Dp�fDqfDq�fDr  Dr�fDs  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw�fDy�{D�7�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�z@ǮA�
A#�
AC�
Ac�
A��A��A��A��A¸RA��A��A��B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��Ba\)Bi\)Bp�]Bx��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�G�B�z�B�z�B�z�B�z�B�z�B�G�B�z�B�z�B�z�B�z�B�z�B�z�BЮBԮB�z�B�z�B�z�B�z�B�z�B�z�B�z�B�G�B�z�B�z�C =qC=qC=qC=qC=qC
=qC=qC=qC=qCWC=qC=qC=qC=qC=qC=qC =qC"WC$WC&=qC(=qC*=qC,=qC.=qC0=qC2=qC4=qC6=qC8=qC:=qC<=qC>=qC@=qCB=qCD#�CF=qCH=qCJ=qCL=qCN=qCP=qCR=qCT=qCV=qCX=qCZ=qC\=qC^=qC`=qCb=qCd=qCf=qChWCj=qCl=qCn=qCp=qCr=qCt=qCv=qCx=qCz=qC|=qC~#�C��C��C��C��C��C�+�C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C�+�C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C�+�C��C��C��C��C�+�D �D ��D\D�\D\D�\D\D�\D\D�\D�D��D�D�\D\D��D�D��D	�D	��D
\D
�\D\D��D�D��D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D�D��D\D�\D\D��D�D�\D\D��D\D��D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)�D)�\D*\D*�\D+\D+�\D,\D,��D-\D-�\D.�D.��D/\D/�\D0\D0�\D1\D1��D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE�DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI��DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN�DN�\DO\DO�\DP\DP��D\��D]�D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df��Dg�Dg�\Dh\Dh�\Di�Di��Dj\Dj�\Dk\Dk�\Dl�Dl��Dm\Dm�\Dn\Dn�\Do\Do��Dp\Dp��Dq�Dq��Dr\Dr��Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dw��Dy��D�?\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��A��A��A��A�oA�{A��A� �A� �A� �A�"�A�$�A�$�A�&�A�&�A��A��Aݕ�A�\)Aە�A���A�?}A�VA֣�A�ZA���A�A�A�K�A���A�1A�M�A�ZAʺ^A�z�A�=qA�v�A�;dAȥ�A�5?A�E�A�{A�ZA��#A�A��A�/A���A�jA�;dA��7A�/A�{A���A�I�A��HA�Q�A��`A��A���A���A��A��uA�bA���A�$�A�=qA��\A�`BA�7LA��hA���A���A�p�A�O�A��A��`A��jA�bNA��#A�?}A�~�A�~�A��A�ffA�VA��A�A�A���A�ffA���A�%A��A�/A�7LA�ZA�XA�bA~��A|I�A{�AzjAv�Au�AtjAt1'As�PAr{Aol�AlM�Aj��Ag��Af$�AeG�AdE�Aa�A[AYl�AXA�AU�AT(�ARQ�AO%AL5?AI��AG�AF^5AE��AD��ABĜA@��A=hsA<ĜA;�A9�FA9�A8�!A8v�A8$�A7t�A4�!A2-A1��A/�A.�9A,E�A+XA*�RA)dZA(bNA'|�A&�\A%�FA$ĜA"��A" �A!|�A   An�A-A��A�^A?}A��A��A^5AƨA��AA��A�A{AO�A�An�A�A  At�A
=A��AAVAG�A
�A	+A��AVAA�!A+A��A�#A��A`BA �A ��@�`B@�b@�-@���@�@�b@�+@��@��
@�!@���@�V@ꟾ@�/@�ƨ@�@�v�@�^@�z�@�ƨ@�F@��@�@���@�@�j@��y@�X@�Q�@�"�@ڏ\@�`B@�j@׮@���@�`B@��`@��;@�
=@��@҇+@���@�l�@�~�@��#@��
@�ff@�-@�{@�J@�@���@��T@ɺ^@�%@�Z@��@ǶF@�C�@�=q@�Ĝ@�bN@\@�ȴ@���@��H@���@��m@��@�\)@�n�@��T@�5?@��\@��R@��R@��\@���@�%@���@�1'@��;@�\)@�l�@�ƨ@�b@� �@�r�@��/@��@�z�@��H@�p�@�G�@��`@��@���@���@���@�I�@��m@��;@�t�@�;d@�+@��@�M�@��u@�1@��@�|�@�S�@���@�-@��T@��7@�/@��@�z�@��@��F@�t�@�;d@��@�~�@�^5@�-@��T@���@�p�@��`@�j@� �@��@��@�dZ@�;d@��H@�M�@��T@���@�`B@��@�Z@��m@��@�ƨ@���@���@�ȴ@���@�ȴ@���@��+@�{@��#@���@�p�@��/@�j@�Z@�bN@�bN@�Q�@�9X@�b@��m@���@�\)@�C�@�ȴ@��\@�J@���@�p�@�p�@�`B@�O�@�&�@���@��/@���@��@��D@�bN@�(�@��F@�33@��@���@��\@�n�@�V@�@�7L@��@��
@��@�33@�@��@���@�V@��@�G�@�Ĝ@�Z@��@�b@�b@�b@�b@�b@�b@��
@�33@�o@�
=@��@��#@��@�O�@�?}@��u@�bN@�Z@�Q�@�9X@� �@���@���@��@�t�@�K�@���@���@�M�@��@�O�@�V@�V@���@��9@�r�@�(�@� �@� �@�b@��m@��
@��
@���@��@�S�@�;d@��@��!@���@�E�@�$�@�@���@��@��#@�@��h@��@��@���@�j@�1'@��@��@��
@���@�֡@q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A��A��A��A�oA�{A��A� �A� �A� �A�"�A�$�A�$�A�&�A�&�A��A��Aݕ�A�\)Aە�A���A�?}A�VA֣�A�ZA���A�A�A�K�A���A�1A�M�A�ZAʺ^A�z�A�=qA�v�A�;dAȥ�A�5?A�E�A�{A�ZA��#A�A��A�/A���A�jA�;dA��7A�/A�{A���A�I�A��HA�Q�A��`A��A���A���A��A��uA�bA���A�$�A�=qA��\A�`BA�7LA��hA���A���A�p�A�O�A��A��`A��jA�bNA��#A�?}A�~�A�~�A��A�ffA�VA��A�A�A���A�ffA���A�%A��A�/A�7LA�ZA�XA�bA~��A|I�A{�AzjAv�Au�AtjAt1'As�PAr{Aol�AlM�Aj��Ag��Af$�AeG�AdE�Aa�A[AYl�AXA�AU�AT(�ARQ�AO%AL5?AI��AG�AF^5AE��AD��ABĜA@��A=hsA<ĜA;�A9�FA9�A8�!A8v�A8$�A7t�A4�!A2-A1��A/�A.�9A,E�A+XA*�RA)dZA(bNA'|�A&�\A%�FA$ĜA"��A" �A!|�A   An�A-A��A�^A?}A��A��A^5AƨA��AA��A�A{AO�A�An�A�A  At�A
=A��AAVAG�A
�A	+A��AVAA�!A+A��A�#A��A`BA �A ��@�`B@�b@�-@���@�@�b@�+@��@��
@�!@���@�V@ꟾ@�/@�ƨ@�@�v�@�^@�z�@�ƨ@�F@��@�@���@�@�j@��y@�X@�Q�@�"�@ڏ\@�`B@�j@׮@���@�`B@��`@��;@�
=@��@҇+@���@�l�@�~�@��#@��
@�ff@�-@�{@�J@�@���@��T@ɺ^@�%@�Z@��@ǶF@�C�@�=q@�Ĝ@�bN@\@�ȴ@���@��H@���@��m@��@�\)@�n�@��T@�5?@��\@��R@��R@��\@���@�%@���@�1'@��;@�\)@�l�@�ƨ@�b@� �@�r�@��/@��@�z�@��H@�p�@�G�@��`@��@���@���@���@�I�@��m@��;@�t�@�;d@�+@��@�M�@��u@�1@��@�|�@�S�@���@�-@��T@��7@�/@��@�z�@��@��F@�t�@�;d@��@�~�@�^5@�-@��T@���@�p�@��`@�j@� �@��@��@�dZ@�;d@��H@�M�@��T@���@�`B@��@�Z@��m@��@�ƨ@���@���@�ȴ@���@�ȴ@���@��+@�{@��#@���@�p�@��/@�j@�Z@�bN@�bN@�Q�@�9X@�b@��m@���@�\)@�C�@�ȴ@��\@�J@���@�p�@�p�@�`B@�O�@�&�@���@��/@���@��@��D@�bN@�(�@��F@�33@��@���@��\@�n�@�V@�@�7L@��@��
@��@�33@�@��@���@�V@��@�G�@�Ĝ@�Z@��@�b@�b@�b@�b@�b@�b@��
@�33@�o@�
=@��@��#@��@�O�@�?}@��u@�bN@�Z@�Q�@�9X@� �@���@���@��@�t�@�K�@���@���@�M�@��@�O�@�V@�V@���@��9@�r�@�(�@� �@� �@�b@��m@��
@��
@���@��@�S�@�;d@��@��!@���@�E�@�$�@�@���@��@��#@�@��h@��@��@���@�j@�1'@��@��@��
@���@�֡@q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BhsBhsBhsBhsBhsBhsBhsBhsBhsBhsBhsBhsBhsBhsBhsBhsBhsBgmBgmBffBe`BbNBbNBS�BVB^5BcTBe`BjB~�B|�Bw�B|�B�B�JB��BɺB�TB	7B1'B2-BD�B]/B\)BW
BP�B@�B<jB<jB-B�BoBhBhB�B�B�BoB�B!�B&�B-B$�B�BoB  B�B�`BȴB�-B��B{�Bl�Be`BH�B6FB49B-B49BG�BYBcTB]/BE�B)�B!�BB
��B
�dB
�B
�B
�B
YB
YB
_;B
e`B
�uB
�B
`BB
6FB
)�B
33B
-B
�B
1B
  B
B
  B	�B	�B	�}B	�'B	��B	��B	��B	��B	�B	dZB	VB	N�B	@�B	9XB	.B	�B	uB	
=B	  B��B��B�B�ZB�
BɺBĜB��B�dB�XB�LB�FB�3B�B��B��B��B��B��B��B�uB�hB�\B�PB�DB�=B�+B�B�B� B~�B}�B~�B}�B|�B|�B|�B{�B{�Bz�Bw�Bz�B{�B}�B~�B�B�B�B�1B�VB�\B�\B�VB�PB�VB�{B��B��B��B��B��B��B��B��B��B�oB�oB�bB�PB�+B� B|�Bz�Bz�Bz�B}�B}�B� B�B�B�B� B�=B�hB��B��B��B��B��B��B��B��B��B��B�B�B�!B�-B�9B�FB�dB��BBŢB��B��B��B��B��B��B��B��B�#B�5B�5B�NB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�TB�ZB�`B�ZB�TB�TB�HB�fB�B�B�B��B��B�B�B��B��B	B	B		7B	VB	bB	bB	bB	uB	�B	�B	�B	 �B	$�B	)�B	.B	49B	:^B	@�B	G�B	K�B	J�B	F�B	J�B	K�B	K�B	N�B	O�B	Q�B	W
B	[#B	[#B	`BB	bNB	dZB	dZB	dZB	cTB	aHB	aHB	cTB	dZB	e`B	iyB	o�B	r�B	v�B	}�B	�B	�+B	�JB	�PB	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�3B	�LB	�RB	�XB	�dB	�jB	�qB	�wB	��B	ĜB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�)B	�)B	�)B	�/B	�/B	�/B	�5B	�5B	�5B	�5B	�;B	�;B	�HB	�NB	�NB	�NB	�NB	�NB	�TB	�ZB	�TB	�NB	�HB	�BB	�BB	�HB	�NB	�NB	�NB	�ZB	�`B	�mB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
	7B
	7B
	7B
	7B

=B

=B
DB
DB
DB
DB
JB
JB
JB
JB
JB
PB
PB
VB
VB
VB
\B
\B
\B
\B
\B
\B
bB
bB
bB
hB
oB
oB
oB
oB
uB
uB
{B
�B
(
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BhsBhsBhsBhsBhsBhsBhsBhsBhsBhsBhsBhsBhsBhsBhsBhsBhsBgmBgmBffBe`BbNBbNBS�BVB^5BcTBe`BjB~�B|�Bw�B|�B�B�JB��BɺB�TB	7B1'B2-BD�B]/B\)BW
BP�B@�B<jB<jB-B�BoBhBhB�B�B�BoB�B!�B&�B-B$�B�BoB  B�B�`BȴB�-B��B{�Bl�Be`BH�B6FB49B-B49BG�BYBcTB]/BE�B)�B!�BB
��B
�dB
�B
�B
�B
YB
YB
_;B
e`B
�uB
�B
`BB
6FB
)�B
33B
-B
�B
1B
  B
B
  B	�B	�B	�}B	�'B	��B	��B	��B	��B	�B	dZB	VB	N�B	@�B	9XB	.B	�B	uB	
=B	  B��B��B�B�ZB�
BɺBĜB��B�dB�XB�LB�FB�3B�B��B��B��B��B��B��B�uB�hB�\B�PB�DB�=B�+B�B�B� B~�B}�B~�B}�B|�B|�B|�B{�B{�Bz�Bw�Bz�B{�B}�B~�B�B�B�B�1B�VB�\B�\B�VB�PB�VB�{B��B��B��B��B��B��B��B��B��B�oB�oB�bB�PB�+B� B|�Bz�Bz�Bz�B}�B}�B� B�B�B�B� B�=B�hB��B��B��B��B��B��B��B��B��B��B�B�B�!B�-B�9B�FB�dB��BBŢB��B��B��B��B��B��B��B��B�#B�5B�5B�NB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�TB�ZB�`B�ZB�TB�TB�HB�fB�B�B�B��B��B�B�B��B��B	B	B		7B	VB	bB	bB	bB	uB	�B	�B	�B	 �B	$�B	)�B	.B	49B	:^B	@�B	G�B	K�B	J�B	F�B	J�B	K�B	K�B	N�B	O�B	Q�B	W
B	[#B	[#B	`BB	bNB	dZB	dZB	dZB	cTB	aHB	aHB	cTB	dZB	e`B	iyB	o�B	r�B	v�B	}�B	�B	�+B	�JB	�PB	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�3B	�LB	�RB	�XB	�dB	�jB	�qB	�wB	��B	ĜB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�)B	�)B	�)B	�/B	�/B	�/B	�5B	�5B	�5B	�5B	�;B	�;B	�HB	�NB	�NB	�NB	�NB	�NB	�TB	�ZB	�TB	�NB	�HB	�BB	�BB	�HB	�NB	�NB	�NB	�ZB	�`B	�mB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
	7B
	7B
	7B
	7B

=B

=B
DB
DB
DB
DB
JB
JB
JB
JB
JB
PB
PB
VB
VB
VB
\B
\B
\B
\B
\B
\B
bB
bB
bB
hB
oB
oB
oB
oB
uB
uB
{B
�B
(
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.24 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140821                              AO  ARCAADJP                                                                    20181024140821    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140821  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140821  QCF$                G�O�G�O�G�O�0               