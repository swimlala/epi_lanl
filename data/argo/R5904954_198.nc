CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:34Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191734  20181005191734  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @����Q;�1   @���\�6�@6B��`A��d��t�j1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  A�33B  BffB  B   B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B���C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�fC   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR�CT�CV  CX  CZ  C\�C^�C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr�Ct  Cv  Cx  Cz�C|  C~  C��C�  C�  C�  C�  C��C��C��C��C�  C��C�  C��3C��3C�  C�  C��C�  C�  C�  C�  C��C��C�  C�  C��3C��3C��3C�  C�  C�  C�  C��3C�  C��3C��3C��3C��3C�  C��C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��C�  C��C��3C��C�  C�  C�  C�  C�  C��3C��3C��fC��3C�  C��C��C��C��C�  C�  C�  C��C�  C�  C�  C�  C��C�  C��3C��3C��3C��C��C��3C�  C�  C��C�  C�  C��C�  C�  C��C�  C��3C��fC�  C��C�  C��3C��3C��3C�  C�  C�  C��C��C��C�  C�  C��C�  C��C�  C��3C��C�  C��C��C��C��C�  C�  C��D   D y�D� DfD�fDfD�fDfD� D  D� D  D� D  D� D  D� D  Dy�D��D� D  D� DfDy�D  D�fDfDy�D  D� D  D� D��Dy�D��D� D  Dy�D  D� D   D y�D!  D!y�D!��D"� D#  D#y�D$  D$� D%  D%�fD&fD&� D'  D'� D(  D(� D)fD)� D)��D*� D*��D+� D,fD,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D2��D3� D4  D4� D4��D5y�D6  D6�fD7  D7� D8  D8y�D8��D9y�D:  D:�fD;fD;� D<  D<� D<��D=� D>fD>�fD?fD?� D@  D@� DA  DAy�DB  DB� DC  DCy�DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DIy�DJ  DJ� DK  DK�fDL  DL� DM  DM� DN  DN� DO  DO� DPfDP�fDQ  DQ� DRfDR� DS  DS� DT  DT� DU  DU� DV  DV�fDW  DW� DW��DX� DY  DY� DZ  DZ� D[  D[� D\fD\� D]  D]� D^  D^� D_fD_�fD`fD`� D`��Day�Db  Dby�Db��Dc� DdfDd� De  De� Df  Df� Df��Dg� Dh  Dhy�Dh��Di� DjfDj� Dk  Dk�fDlfDl� Dl��Dmy�Dm��Dn� Do  Doy�Dp  Dpy�Dq  Dq� DrfDr�fDs�Ds� Dt  Dt�fDu  Duy�Du��Dv� DwfDw` Dyl)D�3�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�G�@��HAp�A%p�AEp�Aep�A��RA��RA��RA��RA¸RAҸRA�RA�RB ��B	\)BB\)B!\)B)\)B1\)B9\)B@��BI\)BQ\)BY\)Ba\)Bi\)Bp��By\)B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BĮBȮB̮BЮBԮBخBܮB�B�B�B�B��GB��GB��B��C =qCW
CW
CW
CW
C
W
CW
CW
CW
CW
CW
CW
CW
CW
CW
C=pC W
C"W
C$W
C&W
C(W
C*W
C,W
C.W
C0W
C2W
C4W
C6W
C8W
C:W
C<W
C>W
C@W
CBW
CD=pCFW
CHW
CJW
CLW
CNW
CPW
CRp�CTp�CVW
CXW
CZW
C\p�C^p�C`W
CbW
CdW
CfW
Chp�CjW
ClW
CnW
CpW
Crp�CtW
CvW
CxW
Czp�C|W
C~W
C�8RC�+�C�+�C�+�C�+�C�8RC�8RC�8RC�8RC�+�C�8RC�+�C��C��C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�8RC�8RC�+�C�+�C��C��C��C�+�C�+�C�+�C�+�C��C�+�C��C��C��C��C�+�C�8RC�8RC�8RC�+�C��C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C��C�8RC�+�C�8RC��C�8RC�+�C�+�C�+�C�+�C�+�C��C��C��C��C�+�C�8RC�8RC�8RC�EC�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�8RC�+�C��C��C��C�8RC�8RC��C�+�C�+�C�8RC�+�C�+�C�8RC�+�C�+�C�8RC�+�C��C��C�+�C�8RC�+�C��C��C��C�+�C�+�C�+�C�8RC�8RC�EC�+�C�+�C�8RC�+�C�8RC�+�C��C�8RC�+�C�8RC�EC�8RC�8RC�+�C�+�C�8RD �D �]D��D)D�)D)D�)D)D��D�D��D�D��D�D��D�D��D�D�]D]D��D�D��D)D�]D�D�)D)D�]D�D��D�D��D]D�]D]D��D�D�]D�D��D �D �]D!�D!�]D"]D"��D#�D#�]D$�D$��D%�D%�)D&)D&��D'�D'��D(�D(��D))D)��D*]D*��D+]D+��D,)D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3]D3��D4�D4��D5]D5�]D6�D6�)D7�D7��D8�D8�]D9]D9�]D:�D:�)D;)D;��D<�D<��D=]D=��D>)D>�)D?)D?��D@�D@��DA�DA�]DB�DB��DC�DC�]DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI�]DJ�DJ��DK�DK�)DL�DL��DM�DM��DN�DN��DO�DO��DP)DP�)DQ�DQ��DR)DR��DS�DS��DT�DT��DU�DU��DV�DV�)DW�DW��DX]DX��DY�DY��DZ�DZ��D[�D[��D\)D\��D]�D]��D^�D^��D_)D_�)D`)D`��Da]Da�]Db�Db�]Dc]Dc��Dd)Dd��De�De��Df�Df��Dg]Dg��Dh�Dh�]Di]Di��Dj)Dj��Dk�Dk�)Dl)Dl��Dm]Dm�]Dn]Dn��Do�Do�]Dp�Dp�]Dq�Dq��Dr)Dr�)Ds"�Ds��Dt�Dt�)Du�Du�]Dv]Dv��Dw)Dwu�Dy��D�>fD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��A��A��A��A��A��TA��A��A��A��#A��A���A���A�ĜA̼jA̸RA̶FA̲-Ạ�A̗�A�|�A�jA�Q�A�O�A�I�A�&�A�1Aˉ7A�%A���A���A��#A���Aʴ9AʋDA�oA�M�A���A���Aȡ�Aȗ�A��A��Aǉ7A�A�A��AƟ�A�ZA��AŋDA�$�Aß�A�9XA�(�A��PA�A�A�+A���A�1A�dZA�$�A��A���A���A��A��mA���A�/A�ƨA�ȴA��A�z�A��A�+A�A���A���A��`A�ƨA�hsA�ƨA�XA��7A�  A�JA�dZA�oA���A��
A�+A�G�A�?}A���A�A�ĜA���A��DA��DA��A�33A��A��A��TA�|�A�VA�x�A�A�\)A�"�A� �A�x�A�n�A�"�A�=qA��PA�"�A�9XA�bA~M�A}K�A|ȴA|ZA|9XA{��A{VAw�#As�
ArE�ApĜAml�Aj��Ai�;AiXAhjAg�Af��Ad��Ac�;Ab�AaG�A]��A\�RAX��AVĜAU��AT�\AS�^AR-AP�HAO�AO?}ANn�AN5?AN�AM��AMS�AL-AKVAH��AE�
AC��AB�AAA@9XA?��A>ĜA>�DA=ƨA<5?A:~�A:$�A9ƨA9%A7�A6M�A5�;A4��A3/A1�A/�A/VA.n�A-��A+;dA*~�A)x�A(�A'C�A%��A$I�A#&�A!�A!VA �`A -A�AO�A�A��AVA��A�9A�FA�A �A�A��Al�A��AVA��A"�Ax�Ar�A��A	�TA	hsAv�A�Al�A��A�A~�AA�+Ap�A�`A�9A��AjAA��AO�A ��A ZA   @��T@�9X@��@��
@���@�|�@��H@�9X@��#@�|�@��^@���@��`@�(�@���@�7L@睲@��@�1'@�-@őh@�&�@�Q�@�Ĝ@�9X@�z�@�b@�  @���@���@���@��T@���@��^@�7L@���@��@��@�l�@�\)@�
=@�v�@���@��@�{@�@�p�@���@�(�@�\)@�Ĝ@�bN@��@�b@��@�\)@�+@���@��P@�ƨ@�1'@���@�"�@�{@�x�@���@��D@���@�O�@�O�@��m@��@��@��@��T@�~�@�@�
=@���@�ȴ@��@��u@���@�Z@��@��F@�S�@�+@��H@���@�$�@��^@�?}@�7L@��@��@�1@��F@�t�@�dZ@�S�@�C�@�33@�@���@�n�@���@���@���@��7@�x�@�O�@��/@��@�bN@�Q�@�I�@�I�@�I�@���@�t�@�33@��+@�J@��T@�@���@��@�hs@�7L@�9X@���@��H@�~�@�M�@�=q@�^5@�ff@��7@���@�j@�b@���@�|�@�C�@�+@�+@��@�
=@���@�M�@�{@���@�x�@�X@���@���@�r�@�Z@�A�@�9X@�1'@���@���@�dZ@�"�@���@���@�v�@�ff@�5?@��@���@�@��-@�X@��@�%@��@��@��;@�dZ@�o@��!@�n�@�E�@�-@�J@��@�=q@�V@�^5@�{@��T@���@��@��@��9@�bN@�  @�ƨ@��@���@��@�K�@�"�@���@���@�{@�@�p�@�%@��j@���@��D@�Q�@��@��
@���@��@�t�@�C�@�o@��@��R@�E�@�-@�J@��-@�x�@�O�@�V@��j@��@|S�@d��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A��A��A��A��A��TA��A��A��A��#A��A���A���A�ĜA̼jA̸RA̶FA̲-Ạ�A̗�A�|�A�jA�Q�A�O�A�I�A�&�A�1Aˉ7A�%A���A���A��#A���Aʴ9AʋDA�oA�M�A���A���Aȡ�Aȗ�A��A��Aǉ7A�A�A��AƟ�A�ZA��AŋDA�$�Aß�A�9XA�(�A��PA�A�A�+A���A�1A�dZA�$�A��A���A���A��A��mA���A�/A�ƨA�ȴA��A�z�A��A�+A�A���A���A��`A�ƨA�hsA�ƨA�XA��7A�  A�JA�dZA�oA���A��
A�+A�G�A�?}A���A�A�ĜA���A��DA��DA��A�33A��A��A��TA�|�A�VA�x�A�A�\)A�"�A� �A�x�A�n�A�"�A�=qA��PA�"�A�9XA�bA~M�A}K�A|ȴA|ZA|9XA{��A{VAw�#As�
ArE�ApĜAml�Aj��Ai�;AiXAhjAg�Af��Ad��Ac�;Ab�AaG�A]��A\�RAX��AVĜAU��AT�\AS�^AR-AP�HAO�AO?}ANn�AN5?AN�AM��AMS�AL-AKVAH��AE�
AC��AB�AAA@9XA?��A>ĜA>�DA=ƨA<5?A:~�A:$�A9ƨA9%A7�A6M�A5�;A4��A3/A1�A/�A/VA.n�A-��A+;dA*~�A)x�A(�A'C�A%��A$I�A#&�A!�A!VA �`A -A�AO�A�A��AVA��A�9A�FA�A �A�A��Al�A��AVA��A"�Ax�Ar�A��A	�TA	hsAv�A�Al�A��A�A~�AA�+Ap�A�`A�9A��AjAA��AO�A ��A ZA   @��T@�9X@��@��
@���@�|�@��H@�9X@��#@�|�@��^@���@��`@�(�@���@�7L@睲@��@�1'@�-@őh@�&�@�Q�@�Ĝ@�9X@�z�@�b@�  @���@���@���@��T@���@��^@�7L@���@��@��@�l�@�\)@�
=@�v�@���@��@�{@�@�p�@���@�(�@�\)@�Ĝ@�bN@��@�b@��@�\)@�+@���@��P@�ƨ@�1'@���@�"�@�{@�x�@���@��D@���@�O�@�O�@��m@��@��@��@��T@�~�@�@�
=@���@�ȴ@��@��u@���@�Z@��@��F@�S�@�+@��H@���@�$�@��^@�?}@�7L@��@��@�1@��F@�t�@�dZ@�S�@�C�@�33@�@���@�n�@���@���@���@��7@�x�@�O�@��/@��@�bN@�Q�@�I�@�I�@�I�@���@�t�@�33@��+@�J@��T@�@���@��@�hs@�7L@�9X@���@��H@�~�@�M�@�=q@�^5@�ff@��7@���@�j@�b@���@�|�@�C�@�+@�+@��@�
=@���@�M�@�{@���@�x�@�X@���@���@�r�@�Z@�A�@�9X@�1'@���@���@�dZ@�"�@���@���@�v�@�ff@�5?@��@���@�@��-@�X@��@�%@��@��@��;@�dZ@�o@��!@�n�@�E�@�-@�J@��@�=q@�V@�^5@�{@��T@���@��@��@��9@�bN@�  @�ƨ@��@���@��@�K�@�"�@���@���@�{@�@�p�@�%@��j@���@��D@�Q�@��@��
@���@��@�t�@�C�@�o@��@��R@�E�@�-@�J@��-@�x�@�O�@�V@��j@��@|S�@d��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B$�B,B49BD�BL�BP�BP�BP�BVBYBW
BN�BP�BXB_;BaHBcTBdZBiyBo�Bs�Br�Bp�Bp�Bw�Bw�B�B�DB��B��B��B��B��B��B��B��B��B��B��B�B�9B�'B��B��B��B��B�qB��B��B��B�{B�oB�VB�+B�B� By�Bw�Bt�Bk�B_;BQ�BF�B@�B>wB9XB5?B2-B1'B.B,BB�)B��B�wB�-B��B��B�hB�Bk�BcTBW
B>wB�B+B
�B
�
B
��B
ĜB
�dB
�XB
�dB
��B
�B
ȴB
�dB
�B
��B
��B
�%B
y�B
r�B
n�B
k�B
iyB
e`B
]/B
J�B
0!B
$�B
�B
B	�B	�B	�mB	�HB	�#B	��B	��B	B	�LB	�B	��B	�bB	w�B	jB	cTB	\)B	T�B	J�B	C�B	<jB	8RB	5?B	6FB	6FB	6FB	33B	,B	$�B	�B	
=B��B��B��B�B�B�B�B�B�fB�`B�`B�`B�TB�5B�B�
B��B��BƨBB�wB�jB�RB�-B�B�B��B��B��B��B��B��B�{B�uB�hB�\B�JB�7B�B�B� B|�Bz�Bx�Bv�Bt�Br�Bo�Bl�BiyBgmBe`B`BB]/BZBVBT�BS�BS�BR�BR�BQ�BQ�BP�BP�BO�BN�BL�BK�BJ�BK�BL�BR�BT�BS�BW
BYBR�BQ�BQ�BP�BO�BR�BffBcTB]/BYBS�BK�BE�BG�BL�BO�BP�BjBjBl�Bl�BiyBe`Be`BjBs�By�B{�Bw�By�B}�B�B�7B�1B�1B�DB�DB�PB�\B�oB�uB�uB��B��B��B�{B�oB�bB�hB�\B�\B��B��B��B��B��B�B�9B�^BŢBɺB��B��B��B�B�HB�mB�B�B�B�B�B��B��B	PB	uB	{B	�B	�B	"�B	'�B	.B	33B	49B	5?B	;dB	=qB	>wB	>wB	@�B	G�B	K�B	L�B	O�B	P�B	VB	XB	YB	YB	YB	YB	ZB	[#B	\)B	]/B	`BB	bNB	cTB	dZB	dZB	e`B	ffB	gmB	hsB	hsB	hsB	iyB	l�B	m�B	m�B	m�B	n�B	s�B	v�B	z�B	}�B	�B	�B	�B	�B	�B	�%B	�%B	�%B	�DB	�\B	�hB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�-B	�-B	�3B	�?B	�FB	�FB	�LB	�RB	�XB	�^B	�^B	�dB	�}B	�}B	��B	��B	��B	ĜB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�5B	�;B	�;B	�;B	�;B	�BB	�HB	�NB	�TB	�TB	�ZB	�`B	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	��B
	lB
a222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B$�B,B49BD�BL�BP�BP�BP�BVBYBW
BN�BP�BXB_;BaHBcTBdZBiyBo�Bs�Br�Bp�Bp�Bw�Bw�B�B�DB��B��B��B��B��B��B��B��B��B��B��B�B�9B�'B��B��B��B��B�qB��B��B��B�{B�oB�VB�+B�B� By�Bw�Bt�Bk�B_;BQ�BF�B@�B>wB9XB5?B2-B1'B.B,BB�)B��B�wB�-B��B��B�hB�Bk�BcTBW
B>wB�B+B
�B
�
B
��B
ĜB
�dB
�XB
�dB
��B
�B
ȴB
�dB
�B
��B
��B
�%B
y�B
r�B
n�B
k�B
iyB
e`B
]/B
J�B
0!B
$�B
�B
B	�B	�B	�mB	�HB	�#B	��B	��B	B	�LB	�B	��B	�bB	w�B	jB	cTB	\)B	T�B	J�B	C�B	<jB	8RB	5?B	6FB	6FB	6FB	33B	,B	$�B	�B	
=B��B��B��B�B�B�B�B�B�fB�`B�`B�`B�TB�5B�B�
B��B��BƨBB�wB�jB�RB�-B�B�B��B��B��B��B��B��B�{B�uB�hB�\B�JB�7B�B�B� B|�Bz�Bx�Bv�Bt�Br�Bo�Bl�BiyBgmBe`B`BB]/BZBVBT�BS�BS�BR�BR�BQ�BQ�BP�BP�BO�BN�BL�BK�BJ�BK�BL�BR�BT�BS�BW
BYBR�BQ�BQ�BP�BO�BR�BffBcTB]/BYBS�BK�BE�BG�BL�BO�BP�BjBjBl�Bl�BiyBe`Be`BjBs�By�B{�Bw�By�B}�B�B�7B�1B�1B�DB�DB�PB�\B�oB�uB�uB��B��B��B�{B�oB�bB�hB�\B�\B��B��B��B��B��B�B�9B�^BŢBɺB��B��B��B�B�HB�mB�B�B�B�B�B��B��B	PB	uB	{B	�B	�B	"�B	'�B	.B	33B	49B	5?B	;dB	=qB	>wB	>wB	@�B	G�B	K�B	L�B	O�B	P�B	VB	XB	YB	YB	YB	YB	ZB	[#B	\)B	]/B	`BB	bNB	cTB	dZB	dZB	e`B	ffB	gmB	hsB	hsB	hsB	iyB	l�B	m�B	m�B	m�B	n�B	s�B	v�B	z�B	}�B	�B	�B	�B	�B	�B	�%B	�%B	�%B	�DB	�\B	�hB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�-B	�-B	�3B	�?B	�FB	�FB	�LB	�RB	�XB	�^B	�^B	�dB	�}B	�}B	��B	��B	��B	ĜB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�5B	�;B	�;B	�;B	�;B	�BB	�HB	�NB	�TB	�TB	�ZB	�`B	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	��B
	lB
a222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.34 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191734                              AO  ARCAADJP                                                                    20181005191734    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191734  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191734  QCF$                G�O�G�O�G�O�8000            