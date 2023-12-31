CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:34Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190534  20181005190534  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @����}��1   @���Q���@0�C��%�c�z�G�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�33A�33A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B���B���B�  C   C  C  C  C  C	�fC  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C��C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  D   D y�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� DfD� D  D�fD  D� D  D� D  Dy�D��Dy�D  D� D��D� DfD�fDfD� D��Dy�D  D� D  D� D  D� D  D� D��D� DfD�fDfD�fDfD� D��D y�D!  D!� D"  D"� D#fD#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(�fD)  D)� D*  D*� D+  D+� D,  D,� D-fD-� D-��D.y�D.��D/� D0fD0�fD1  D1� D2fD2� D3  D3� D4  D4�fD5fD5� D6  D6� D7  D7y�D7��D8y�D9  D9�fD:  D:� D;  D;�fD<  D<y�D=  D=�fD>  D>� D?  D?� D@  D@� DA  DAy�DB  DB� DC  DC� DD  DD� DE  DE� DFfDF� DG  DG�fDH  DH� DI  DI� DJ  DJ� DJ��DK� DLfDL� DM  DM� DM��DN� DO  DO� DO��DP� DP��DQ� DRfDR� DS  DSy�DS��DT� DU  DU� DV  DV�fDWfDW� DX  DX� DY  DY� DZ  DZ�fD[  D[�fD\fD\� D\��D]� D^  D^� D_  D_� D_��D`y�D`��Da� Db  Db� Dc  Dc� Dd  Dd� De  Dey�Df  Dfy�Dg  Dg� Dh  Dh�fDi  Diy�Dj  Dj� Dj��Dk� DlfDl� Dl��Dmy�Dn  Dn� Dn��Do� Dp  Dp� DqfDq�fDr  Dr� DsfDs�fDt  Dty�Du  Du� Dv  Dv�fDwfDwy�DyhRD�FfD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@ǮA�
A#�
AC�
Ac�
A��A��A��A��A��A��A��A��B ��B��B��B��B ��B)\)B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�G�B�z�B��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�G�B�z�B�G�B�G�B�z�C =qC=qC=qC=qC=qC
#�C=qC=qC=qC=qC=qC=qC=qC=qC=qC=qC =qC"=qC$=qC&=qC(=qC*=qC,=qC.=qC0=qC2=qC4=qC6=qC8=qC:=qC<=qC>=qC@=qCB=qCD=qCF=qCH=qCJ=qCL=qCN=qCP=qCR=qCTWC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C�+�C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C�+�C�+�C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��D \D ��D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D�D�\D\D��D\D�\D\D�\D\D��D�D��D\D�\D�D�\D�D��D�D�\D�D��D\D�\D\D�\D\D�\D\D�\D�D�\D�D��D�D��D�D�\D �D ��D!\D!�\D"\D"�\D#�D#��D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(��D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-�D-�\D.�D.��D/�D/�\D0�D0��D1\D1�\D2�D2�\D3\D3�\D4\D4��D5�D5�\D6\D6�\D7\D7��D8�D8��D9\D9��D:\D:�\D;\D;��D<\D<��D=\D=��D>\D>�\D?\D?�\D@\D@�\DA\DA��DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF�DF�\DG\DG��DH\DH�\DI\DI�\DJ\DJ�\DK�DK�\DL�DL�\DM\DM�\DN�DN�\DO\DO�\DP�DP�\DQ�DQ�\DR�DR�\DS\DS��DT�DT�\DU\DU�\DV\DV��DW�DW�\DX\DX�\DY\DY�\DZ\DZ��D[\D[��D\�D\�\D]�D]�\D^\D^�\D_\D_�\D`�D`��Da�Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De��Df\Df��Dg\Dg�\Dh\Dh��Di\Di��Dj\Dj�\Dk�Dk�\Dl�Dl�\Dm�Dm��Dn\Dn�\Do�Do�\Dp\Dp�\Dq�Dq��Dr\Dr�\Ds�Ds��Dt\Dt��Du\Du�\Dv\Dv��Dw�Dw��Dyw�D�ND��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aԇ+AԍPAԋDAԇ+AԅAԍPAԋDAԋDAԏ\Aԗ�Aԙ�Aԙ�Aԏ\A�XA�I�A�C�A�C�A�A�A�?}A�A�A�33A��A�oA�%A���A��mA��mA��mA��yA��
A���A���A���AӺ^Aӛ�AӇ+AӅAӇ+A�~�A�|�A�n�A�Q�A�-A�&�A���A��A��;A��/A���A�l�A��/A���A�S�A͋DA�|�Aʝ�A�K�A�I�A�Q�A�&�A��
A��/A�p�A���A¸RA�~�A���A�ffA��A���A�A�A��A���A�dZA�oA���A�-A���A�{A��+A���A�I�A��A�`BA�%A��A�;dA���A���A��A�A���A��A���A�l�A�$�A�5?A��jA���A�XA�33A��A�;dA�&�A���AM�AL5?AKXAJ��AJE�AGC�AE��AD��ADJAAA=��A< �A;x�A;
=A:=qA9G�A89XA7�PA5�hA3��A25?A0��A/S�A/A.�A.  A-+A,{A+`BA*�A*9XA(�uA({A'K�A&��A&VA%�#A%dZA$�HA$Q�A#&�A"1A!?}A!�AA��A�-A�A��A��AS�AO�A9XA\)A�A�AE�AA33AM�AAl�A
(�A	C�A-AXA�yA��A�RAr�AJA�^A|�AC�A�AA�uA�^AoA1@��
@��@�9X@�
=@���@��w@�dZ@�\)@�S�@��
@��@�b@��+@��@��D@�K�@�+@��T@�x�@�?}@�X@�j@��@�C�@��@�j@�x�@�@�Q�@��@�S�@�^5@�j@��D@� �@ާ�@� �@�O�@��m@�1@�\)@�(�@�t�@٩�@�/@ش9@���@���@���@��@�=q@�n�@�n�@�-@��@�@�bN@θR@�$�@�-@��@�S�@Η�@ͺ^@͉7@�?}@�p�@�x�@���@�/@��@�(�@�1'@�1@�t�@�n�@�x�@ǅ@��y@�M�@�@š�@��@� �@��@�b@�O�@���@�?}@�x�@�dZ@���@��H@��R@��\@�J@�p�@�r�@��@��j@�A�@�|�@���@��-@�p�@���@���@�9X@� �@��@�\)@��@��H@���@�V@���@��^@�O�@�7L@�&�@���@��u@��@��F@���@�S�@���@���@���@�G�@�/@�&�@���@�=q@�v�@��!@���@�G�@��@��@���@���@�V@���@���@�l�@�33@�"�@��y@��@�V@�E�@��^@��-@��@��@���@���@��@�7L@���@��u@��j@���@�?}@�`B@�hs@�O�@�V@���@�I�@�ƨ@�o@���@�$�@���@��@��j@��j@� �@�K�@�p�@�1@�@�V@�-@�5?@��@��j@���@���@��@�1'@�9X@�  @��F@��P@�|�@�l�@�C�@�+@��@��@�-@��^@�X@��j@�9X@��@�9X@�bN@�I�@��F@��R@�~�@�v�@�ff@�-@�%@�r�@�  @���@�|�@�l�@�S�@�;d@�"�@��@���@���@�n�@�{@��T@�p�@��@��9@�Z@�(�@�  @�A�@�9X@��w@�K�@��@���@���@��!@���@�n�@�$�@���@��#@��#@�@���@��h@�x�@�/@�V@���@���@�Ĝ@��D@�(�@�ƨ@��@��@��@���@���@�^5@�-@��@��@���@�G�@��@��@�r�@�Z@�1'@���@��F@���@�5?@��@���@��X@n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aԇ+AԍPAԋDAԇ+AԅAԍPAԋDAԋDAԏ\Aԗ�Aԙ�Aԙ�Aԏ\A�XA�I�A�C�A�C�A�A�A�?}A�A�A�33A��A�oA�%A���A��mA��mA��mA��yA��
A���A���A���AӺ^Aӛ�AӇ+AӅAӇ+A�~�A�|�A�n�A�Q�A�-A�&�A���A��A��;A��/A���A�l�A��/A���A�S�A͋DA�|�Aʝ�A�K�A�I�A�Q�A�&�A��
A��/A�p�A���A¸RA�~�A���A�ffA��A���A�A�A��A���A�dZA�oA���A�-A���A�{A��+A���A�I�A��A�`BA�%A��A�;dA���A���A��A�A���A��A���A�l�A�$�A�5?A��jA���A�XA�33A��A�;dA�&�A���AM�AL5?AKXAJ��AJE�AGC�AE��AD��ADJAAA=��A< �A;x�A;
=A:=qA9G�A89XA7�PA5�hA3��A25?A0��A/S�A/A.�A.  A-+A,{A+`BA*�A*9XA(�uA({A'K�A&��A&VA%�#A%dZA$�HA$Q�A#&�A"1A!?}A!�AA��A�-A�A��A��AS�AO�A9XA\)A�A�AE�AA33AM�AAl�A
(�A	C�A-AXA�yA��A�RAr�AJA�^A|�AC�A�AA�uA�^AoA1@��
@��@�9X@�
=@���@��w@�dZ@�\)@�S�@��
@��@�b@��+@��@��D@�K�@�+@��T@�x�@�?}@�X@�j@��@�C�@��@�j@�x�@�@�Q�@��@�S�@�^5@�j@��D@� �@ާ�@� �@�O�@��m@�1@�\)@�(�@�t�@٩�@�/@ش9@���@���@���@��@�=q@�n�@�n�@�-@��@�@�bN@θR@�$�@�-@��@�S�@Η�@ͺ^@͉7@�?}@�p�@�x�@���@�/@��@�(�@�1'@�1@�t�@�n�@�x�@ǅ@��y@�M�@�@š�@��@� �@��@�b@�O�@���@�?}@�x�@�dZ@���@��H@��R@��\@�J@�p�@�r�@��@��j@�A�@�|�@���@��-@�p�@���@���@�9X@� �@��@�\)@��@��H@���@�V@���@��^@�O�@�7L@�&�@���@��u@��@��F@���@�S�@���@���@���@�G�@�/@�&�@���@�=q@�v�@��!@���@�G�@��@��@���@���@�V@���@���@�l�@�33@�"�@��y@��@�V@�E�@��^@��-@��@��@���@���@��@�7L@���@��u@��j@���@�?}@�`B@�hs@�O�@�V@���@�I�@�ƨ@�o@���@�$�@���@��@��j@��j@� �@�K�@�p�@�1@�@�V@�-@�5?@��@��j@���@���@��@�1'@�9X@�  @��F@��P@�|�@�l�@�C�@�+@��@��@�-@��^@�X@��j@�9X@��@�9X@�bN@�I�@��F@��R@�~�@�v�@�ff@�-@�%@�r�@�  @���@�|�@�l�@�S�@�;d@�"�@��@���@���@�n�@�{@��T@�p�@��@��9@�Z@�(�@�  @�A�@�9X@��w@�K�@��@���@���@��!@���@�n�@�$�@���@��#@��#@�@���@��h@�x�@�/@�V@���@���@�Ĝ@��D@�(�@�ƨ@��@��@��@���@���@�^5@�-@��@��@���@�G�@��@��@�r�@�Z@�1'@���@��F@���@�5?@��@���@��X@n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�^B
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�wB
��B
�B
�BB
�ZB
�B
�B
��BB
��B  B  BB  B  B  BB  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
��B
��B
��BBBBB
��B
��B�BXBq�B�B�^BɺB�
B�B�B  BJB�B:^BF�BYBL�B,B$�B �B!�B�B�B�B"�B'�B;dBG�BO�B`BBcTB\)BVBYB\)B]/BW
BF�B:^B!�BVB�mB�dB��B��B�bB�Bp�BM�B>wB2-B{B�PB�=B�1B�+B�B�B~�B}�Bz�Bx�Bw�Bz�Bz�Bz�B{�B}�B� B�B�+B�VB�uB��B��B��B��B��B��B�B�'B�3B�LBÖBŢBƨBȴBɺB��B��B��B��B�
B�)B�5B�/B�BB�)B��B�9B�B��B��B��B��B�{B��B�9B�?B�B��B�B�B�B�B�B�B�B�'B�!B�!B�!B�'B�!B�!B�!B�!B�!B�B�!B�!B�!B�3B�-B�3B�9B�FB�jB�}BBǮB��B�B�B�)B�B�B�
B�B�B�)B�HB�mB�B�B�NB�B�B�yB�B��B�B�B�B�B�B�B�B�fB�TB�HB�fB	+B	PB	DB	%B	B	B	%B	
=B	B	B	+B	
=B	DB	
=B	
=B	1B	%B	B	+B	1B	bB	�B	�B	�B	�B	�B	�B	+B	,B	2-B	7LB	8RB	>wB	?}B	?}B	<jB	:^B	33B	:^B	=qB	=qB	?}B	?}B	;dB	33B	/B	<jB	E�B	G�B	J�B	B�B	49B	5?B	7LB	;dB	@�B	C�B	G�B	K�B	Q�B	T�B	R�B	Q�B	N�B	N�B	S�B	S�B	T�B	[#B	aHB	bNB	cTB	dZB	e`B	gmB	l�B	o�B	s�B	u�B	u�B	v�B	y�B	z�B	{�B	� B	�B	�B	�B	�1B	�JB	�PB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�^B	��B	ĜB	ĜB	ÖB	B	�FB	�B	�RB	�RB	�RB	��B	ȴB	ɺB	ƨB	ĜB	ÖB	B	ŢB	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�#B	�)B	�)B	�B	�B	�B	�#B	�5B	�/B	�B	��B	��B	��B	��B	��B	��B	�B	�HB	�NB	�TB	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
+B
+B
1B
+B
�B
-)222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B
�^B
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�wB
��B
�B
�BB
�ZB
�B
�B
��BB
��B  B  BB  B  B  BB  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
��B
��B
��BBBBB
��B
��B�BXBq�B�B�^BɺB�
B�B�B  BJB�B:^BF�BYBL�B,B$�B �B!�B�B�B�B"�B'�B;dBG�BO�B`BBcTB\)BVBYB\)B]/BW
BF�B:^B!�BVB�mB�dB��B��B�bB�Bp�BM�B>wB2-B{B�PB�=B�1B�+B�B�B~�B}�Bz�Bx�Bw�Bz�Bz�Bz�B{�B}�B� B�B�+B�VB�uB��B��B��B��B��B��B�B�'B�3B�LBÖBŢBƨBȴBɺB��B��B��B��B�
B�)B�5B�/B�BB�)B��B�9B�B��B��B��B��B�{B��B�9B�?B�B��B�B�B�B�B�B�B�B�'B�!B�!B�!B�'B�!B�!B�!B�!B�!B�B�!B�!B�!B�3B�-B�3B�9B�FB�jB�}BBǮB��B�B�B�)B�B�B�
B�B�B�)B�HB�mB�B�B�NB�B�B�yB�B��B�B�B�B�B�B�B�B�fB�TB�HB�fB	+B	PB	DB	%B	B	B	%B	
=B	B	B	+B	
=B	DB	
=B	
=B	1B	%B	B	+B	1B	bB	�B	�B	�B	�B	�B	�B	+B	,B	2-B	7LB	8RB	>wB	?}B	?}B	<jB	:^B	33B	:^B	=qB	=qB	?}B	?}B	;dB	33B	/B	<jB	E�B	G�B	J�B	B�B	49B	5?B	7LB	;dB	@�B	C�B	G�B	K�B	Q�B	T�B	R�B	Q�B	N�B	N�B	S�B	S�B	T�B	[#B	aHB	bNB	cTB	dZB	e`B	gmB	l�B	o�B	s�B	u�B	u�B	v�B	y�B	z�B	{�B	� B	�B	�B	�B	�1B	�JB	�PB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�^B	��B	ĜB	ĜB	ÖB	B	�FB	�B	�RB	�RB	�RB	��B	ȴB	ɺB	ƨB	ĜB	ÖB	B	ŢB	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�#B	�)B	�)B	�B	�B	�B	�#B	�5B	�/B	�B	��B	��B	��B	��B	��B	��B	�B	�HB	�NB	�TB	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
+B
+B
1B
+B
�B
-)222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.24 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190534                              AO  ARCAADJP                                                                    20181005190534    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190534  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190534  QCF$                G�O�G�O�G�O�8000            