CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:07Z creation      
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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005190607  20181005190607  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��i��8M1   @��j33H@28���F�c��^5?}1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   B   B   @@  @�  @���A   A   AA��Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B���B�  B�  C   C  C  C  C  C	�fC�fC  C  C  C  C  C  C  C  C  C �C"�C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Co�fCr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��3C��3C��3C�  C�  C��3C��3C�  C�  C�  C��C��C�  C�  C�  C�  C��C��C��C�  C��3C��3C��3C��3C��3D � D  D� D  D� D  D� D  D� D  D� DfD� D  D� DfD� D��D	� D
  D
y�D  D� D��D� D  Dy�D  D�fD  D� DfD� D��Dy�D  D� D  D� D��D� D  D�fD  Dy�D��D� DfD�fD  D� D  D� DfD� D  D� D  D� D  Dy�D  D� D   D � D!  D!� D!��D"� D#  D#� D#��D$� D%  D%� D&  D&� D'fD'� D(  D(� D)  D)� D*  D*� D+  D+� D+��D,y�D-  D-� D-��D.y�D/  D/� D0  D0y�D1fD1�fD2  D2�fD3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8�fD9  D9� D:  D:� D;  D;� D;��D<� D=fD=�fD>  D>y�D>��D?� D@  D@� DA  DA� DB  DBy�DC  DC� DD  DD� DE  DE� DF  DFy�DG  DG� DH  DH� DI  DIy�DJ  DJ� DK  DK� DL  DL�fDM  DM� DN  DN� DOfDO�fDPfDP�fDQ  DQy�DQ��DR� DS  DS� DT  DT�fDU  DU� DVfDV� DV��DW� DX  DX�fDY  DYy�DZ  DZ�fD[  D[� D[��D\� D]  D]� D]��D^� D_fD_�fD`fD`� D`��Day�Da��Db� Dc  Dc� Dd  Dd� De  De� De��Dfy�Df��Dg� Dh  Dh� DifDi� Dj  Dj� Dj��Dk� DlfDl� Dm  Dm� Dn  Dn�fDofDo�fDp  Dpy�Dp��Dq� Dq��Dr� Dr��Ds� DtfDt�fDu  Du� Du��Dv� Dw  Dw� Dw�3Dy��D�C�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @S33@���@�fgA��A$��AFfgAffgA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B	33B33B33B!33B)33B133B933BA33BI33BQ33BY33Ba33Bi33Bq33By33B���B���B���B�fgB���B���B���B���B���B���B���B���B���B���B���B���B���Bę�Bș�B̙�BЙ�Bԙ�B���B���B���B䙚B虚B왚B�B�fgB���B���C L�CL�CL�CL�CL�C
33C33CL�CL�CL�CL�CL�CL�CL�CL�CL�C fgC"fgC$fgC&L�C(L�C*L�C,L�C.L�C0L�C2L�C4L�C6L�C8L�C:L�C<L�C>L�C@L�CBL�CDL�CFL�CHL�CJL�CLL�CNL�CPL�CRL�CTL�CVL�CXL�CZL�C\L�C^L�C`L�CbL�CdL�CfL�ChL�CjL�ClL�CnL�Cp33CrL�CtL�CvL�CxL�CzL�C|L�C~L�C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC��C��C�&fC�&fC�&fC�&fC��C�&fC�33C�33C�&fC�&fC�&fC�&fC�33C�33C�&fC�&fC�&fC�&fC��C��C��C�&fC�&fC��C��C�&fC�&fC�&fC�33C�33C�&fC�&fC�&fC�&fC�33C�33C�33C�&fC��C��C��C��D �D �3D3D�3D3D�3D3D�3D3D�3D3D�3D�D�3D3D�3D�D�3D	�D	�3D
3D
��D3D�3D�D�3D3D��D3D��D3D�3D�D�3D�D��D3D�3D3D�3D�D�3D3D��D3D��D�D�3D�D��D3D�3D3D�3D�D�3D3D�3D3D�3D3D��D3D�3D 3D �3D!3D!�3D"�D"�3D#3D#�3D$�D$�3D%3D%�3D&3D&�3D'�D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,�D,��D-3D-�3D.�D.��D/3D/�3D03D0��D1�D1��D23D2��D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8��D93D9�3D:3D:�3D;3D;�3D<�D<�3D=�D=��D>3D>��D?�D?�3D@3D@�3DA3DA�3DB3DB��DC3DC�3DD3DD�3DE3DE�3DF3DF��DG3DG�3DH3DH�3DI3DI��DJ3DJ�3DK3DK�3DL3DL��DM3DM�3DN3DN�3DO�DO��DP�DP��DQ3DQ��DR�DR�3DS3DS�3DT3DT��DU3DU�3DV�DV�3DW�DW�3DX3DX��DY3DY��DZ3DZ��D[3D[�3D\�D\�3D]3D]�3D^�D^�3D_�D_��D`�D`�3Da�Da��Db�Db�3Dc3Dc�3Dd3Dd�3De3De�3Df�Df��Dg�Dg�3Dh3Dh�3Di�Di�3Dj3Dj�3Dk�Dk�3Dl�Dl�3Dm3Dm�3Dn3Dn��Do�Do��Dp3Dp��Dq�Dq�3Dr�Dr�3Ds�Ds�3Dt�Dt��Du3Du�3Dv�Dv�3Dw3Dw�3Dw�fDy��D�MqD�Æ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�A�A�A�JA�{A�oA�oA�oA�{A�{A��A��A�oA�%A��A��A�AȲ-Aȩ�Aȥ�Aȥ�Aȣ�Aȝ�AȑhA�v�A���AƓuA��AøRA�v�A�JA�|�A�?}A��A���A�p�A��A��A��#A���A�v�A�dZA���A���A��A�r�A�;dA�-A�JA��jA���A��A�n�A�r�A��A��\A���A�t�A��;A��-A���A�VA��jA���A��mA���A�(�A���A�~�A���A��A�&�A�ZA��A�jA�ZA�ZA�\)A�^5A�^5A��A���A���A��9A��^A�ƨA�33A�(�A���A��HA��A�A�(�A���A��!A��DA���A�\)A��#A���A���A��A�dZA�7LA�+A���A�l�A�ȴA{7LAwK�As�^Am�AlI�Ak��Aj�Ag��AdA�AbQ�A]l�A[x�AZffAY|�AX��AW`BAUO�AQ��AMO�AJ�uAI��AIt�AF�yAD�`AC�AB�jA@�+A>VA:��A8�\A7hsA6Q�A5oA2�`A0$�A/
=A.��A.-A-l�A+��A)��A'O�A%��A$��A#�
A#O�A!�A!�A!%A ffA�-AC�A��Ar�A"�Ax�A�\AbA�hA�HA�wAbA��A^5A�
A��A��A�/A/A��An�A�A  A��A�hA�A��A�#A?}AJA�
A�wA�A"�A
�9A
�DA	�;A�\A��AS�A�DA��A�FA�-Ax�A�AbAhsA �\@���@�C�@�ȴ@�~�@�V@�@��-@�p�@�&�@�Z@� �@��P@���@��m@��+@���@��^@���@�S�@��R@��@��+@��!@��@�@�@�bN@�|�@�K�@�5?@��/@��/@�V@�Ĝ@���@웦@� �@�|�@�~�@�h@�9@�9@�t�@�$�@���@�9@�j@�j@���@���@��@���@���@��@�@�7@�Ĝ@��@ݡ�@���@��/@�Z@�X@ݺ^@���@ڧ�@�^5@�ff@�@�C�@�"�@��H@ڟ�@��@��T@١�@�/@ش9@�r�@�S�@�v�@�ff@�J@Չ7@���@ԛ�@ԃ@�j@�(�@�o@���@�n�@��@�x�@Ь@��@�ƨ@�
=@���@� �@�l�@��@���@��y@���@ʸR@ʏ\@���@ɑh@�&�@Ȭ@ȼj@ɲ-@�hs@�V@��@�b@���@Ɨ�@�E�@Ų-@�G�@��`@ģ�@ļj@ļj@�X@���@�Ĝ@�z�@�  @�l�@��@°!@�@�M�@��#@��7@�x�@�?}@�&�@��@�9X@���@�"�@��R@���@���@��\@��+@�J@��h@�hs@�O�@�G�@�7L@��/@��u@��@�b@��@�33@��y@��\@�ff@�M�@�@��-@��@��j@��@�  @�t�@�\)@�33@�@���@���@��\@�ff@���@���@���@�hs@��`@��u@�  @���@��w@�K�@��@�@��@���@�5?@���@�7L@��@�33@��@�^5@�J@���@�x�@�?}@��/@��j@��u@��u@��@��@�r�@�I�@��@��;@���@��P@�\)@��@���@�n�@�-@�@��@��h@���@��h@�`B@���@�I�@��;@��@�@�ȴ@��R@���@�V@�5?@�{@��#@���@��-@�X@��@�z�@�1'@� �@��
@���@�t�@�C�@�o@��@�n�@�M�@�$�@���@�ƨ@�|�@�K�@�"�@�ȴ@���@��-@���@�G�@��j@�I�@�ƨ@�t�@�l�@�\)@�+@���@�E�@�$�@�@��T@��7@�X@�7L@��9@��@��;@���@��@�ff@��T@���@��}@|��1111111114441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�A�A�A�A�JA�{A�oA�oA�oA�{A�{A��A��A�oA�%A��A��A�AȲ-Aȩ�Aȥ�Aȥ�Aȣ�Aȝ�AȑhA�v�A���AƓuA��AøRA�v�A�JA�|�A�?}A��A���A�p�A��A��A��#A���A�v�A�dZA���A���A��A�r�A�;dA�-A�JA��jA���A��A�n�A�r�A��A��\A���A�t�A��;A��-A���A�VA��jA���A��mA���A�(�A���A�~�A���A��A�&�A�ZA��A�jA�ZA�ZA�\)A�^5A�^5A��A���A���A��9A��^A�ƨA�33A�(�A���A��HA��A�A�(�A���A��!A��DA���A�\)A��#A���A���A��A�dZA�7LA�+A���A�l�A�ȴA{7LAwK�As�^Am�AlI�Ak��Aj�Ag��AdA�AbQ�A]l�A[x�AZffAY|�AX��AW`BAUO�AQ��AMO�AJ�uAI��AIt�AF�yAD�`AC�AB�jA@�+A>VA:��A8�\A7hsA6Q�A5oA2�`A0$�A/
=A.��A.-A-l�A+��A)��A'O�A%��A$��A#�
A#O�A!�A!�A!%A ffA�-AC�A��Ar�A"�Ax�A�\AbA�hA�HA�wAbA��A^5A�
A��A��A�/A/A��An�A�A  A��A�hA�A��A�#A?}AJA�
A�wA�A"�A
�9A
�DA	�;A�\A��AS�A�DA��A�FA�-Ax�A�AbAhsA �\@���@�C�@�ȴ@�~�@�V@�@��-@�p�@�&�@�Z@� �@��P@���@��m@��+@���@��^@���@�S�@��R@��@��+@��!@��@�@�@�bN@�|�@�K�@�5?@��/@��/@�V@�Ĝ@���@웦@� �@�|�@�~�@�h@�9@�9@�t�@�$�@���@�9@�j@�j@���@���@��@���@���@��@�@�7@�Ĝ@��@ݡ�@���@��/@�Z@�X@ݺ^@���@ڧ�@�^5@�ff@�@�C�@�"�@��H@ڟ�@��@��T@١�@�/@ش9@�r�@�S�@�v�@�ff@�J@Չ7@���@ԛ�@ԃ@�j@�(�@�o@���@�n�@��@�x�@Ь@��@�ƨ@�
=@���@� �@�l�@��@���@��y@���@ʸR@ʏ\@���@ɑh@�&�@Ȭ@ȼj@ɲ-@�hs@�V@��@�b@���@Ɨ�@�E�@Ų-@�G�@��`@ģ�@ļj@ļj@�X@���@�Ĝ@�z�@�  @�l�@��@°!@�@�M�@��#@��7@�x�@�?}@�&�@��@�9X@���@�"�@��R@���@���@��\@��+@�J@��h@�hs@�O�@�G�@�7L@��/@��u@��@�b@��@�33@��y@��\@�ff@�M�@�@��-@��@��j@��@�  @�t�@�\)@�33@�@���@���@��\@�ff@���@���@���@�hs@��`@��u@�  @���@��w@�K�@��@�@��@���@�5?@���@�7L@��@�33@��@�^5@�J@���@�x�@�?}@��/@��j@��u@��u@��@��@�r�@�I�@��@��;@���@��P@�\)@��@���@�n�@�-@�@��@��h@���@��h@�`B@���@�I�@��;@��@�@�ȴ@��R@���@�V@�5?@�{@��#@���@��-@�X@��@�z�@�1'@� �@��
@���@�t�@�C�@�o@��@�n�@�M�@�$�@���@�ƨ@�|�@�K�@�"�@�ȴ@���@��-@���@�G�@��j@�I�@�ƨ@�t�@�l�@�\)@�+@���@�E�@�$�@�@��T@��7@�X@�7L@��9@��@��;@���@��@�ff@��T@���@��}@|��1111111114441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	B	B	B	B	B	B	%B	+B		7B	1B�FB\)B	B	\B	)�B	F�B	]/B	e`B	gmB	gmB	hsB	iyB	iyB	iyB	l�B	�B
DB
�B
�bB
�B
�BBBbB�B�B�B(�B,B-B33B6FB;dB9XB8RB49B2-B.B2-B8RB0!B-B/B2-B5?BD�BL�BW
BbNBr�B�B�B�RB��B�;B��B�B�B!�B%�B(�B+B-B6FBN�BgmBp�Bs�Bs�Bt�Bv�B�B�uB��B��B��B�7BYB0!B0!B1'B,B�B�TB�DBL�B�B
��B
ǮB
v�B
�B
B	�B	��B	��B	��B	��B	��B	�DB	e`B	N�B	B�B	7LB	33B	1'B	-B	&�B	�B	�B	B��B�B�B�fB�/B�B��B��B��BɺBƨBɺBɺBȴBƨBŢBBɺB��B��B�B�
B�B�#B�B�B�B�B�B��B��B��B��B�B�B�B�B�B�B�;B�`B�sB�mB�sB�mB�ZB�HB�;B�)B�B�B�#B�#B�B�B�B�HB�B��B��B��B��B��B��B��B	B	B	%B	'�B	&�B	{B	B	1B	�B	!�B	 �B	�B	�B	 �B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	%�B	(�B	)�B	+B	,B	-B	.B	2-B	49B	5?B	9XB	?}B	@�B	?}B	>wB	>wB	>wB	G�B	W
B	XB	aHB	]/B	YB	R�B	S�B	W
B	ZB	[#B	^5B	hsB	r�B	~�B	�B	�1B	�=B	�PB	�VB	�VB	�VB	�VB	�JB	�+B	�B	�B	�B	�%B	�1B	�PB	�\B	�oB	�{B	��B	��B	��B	�oB	�bB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�FB	�RB	�RB	�LB	�LB	�FB	�^B	�RB	�LB	�XB	�^B	�^B	�XB	�^B	�^B	�^B	�^B	�wB	�wB	�}B	��B	��B	ÖB	ĜB	ĜB	ĜB	ƨB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�)B	�/B	�5B	�ZB	�`B	�fB	�fB	�mB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�yB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B	��B	��B
B
B
  B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
	7B

=B
\B

�B
&B
"�2222222224442222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B	B	B	B	B	B	B	%B	+B		7B	1B�FB\)B	B	\B	)�B	F�B	]/B	e`B	gmB	gmB	hsB	iyB	iyB	iyB	l�B	�B
DB
�B
�bB
�B
�BBBbB�B�B�B(�B,B-B33B6FB;dB9XB8RB49B2-B.B2-B8RB0!B-B/B2-B5?BD�BL�BW
BbNBr�B�B�B�RB��B�;B��B�B�B!�B%�B(�B+B-B6FBN�BgmBp�Bs�Bs�Bt�Bv�B�B�uB��B��B��B�7BYB0!B0!B1'B,B�B�TB�DBL�B�B
��B
ǮB
v�B
�B
B	�B	��B	��B	��B	��B	��B	�DB	e`B	N�B	B�B	7LB	33B	1'B	-B	&�B	�B	�B	B��B�B�B�fB�/B�B��B��B��BɺBƨBɺBɺBȴBƨBŢBBɺB��B��B�B�
B�B�#B�B�B�B�B�B��B��B��B��B�B�B�B�B�B�B�;B�`B�sB�mB�sB�mB�ZB�HB�;B�)B�B�B�#B�#B�B�B�B�HB�B��B��B��B��B��B��B��B	B	B	%B	'�B	&�B	{B	B	1B	�B	!�B	 �B	�B	�B	 �B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	%�B	(�B	)�B	+B	,B	-B	.B	2-B	49B	5?B	9XB	?}B	@�B	?}B	>wB	>wB	>wB	G�B	W
B	XB	aHB	]/B	YB	R�B	S�B	W
B	ZB	[#B	^5B	hsB	r�B	~�B	�B	�1B	�=B	�PB	�VB	�VB	�VB	�VB	�JB	�+B	�B	�B	�B	�%B	�1B	�PB	�\B	�oB	�{B	��B	��B	��B	�oB	�bB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�FB	�RB	�RB	�LB	�LB	�FB	�^B	�RB	�LB	�XB	�^B	�^B	�XB	�^B	�^B	�^B	�^B	�wB	�wB	�}B	��B	��B	ÖB	ĜB	ĜB	ĜB	ƨB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�)B	�/B	�5B	�ZB	�`B	�fB	�fB	�mB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�yB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B	��B	��B
B
B
  B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
	7B

=B
\B

�B
&B
"�2222222224442222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190607                              AO  ARCAADJP                                                                    20181005190607    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190607  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190607  QCF$                G�O�G�O�G�O�C000            