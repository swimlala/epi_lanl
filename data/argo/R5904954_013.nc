CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:16:51Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191651  20181005191651  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @צ�d��@1   @צ���|@3�n��P�c�dZ�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B���B�  C 33C�fC  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Cg�fCj  Cl  Cm�fCo�fCq�fCt  Cv�Cx  Cz  C|�C~  C�fC��3C�  C�  C��3C��3C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C��3C��C��C�  C�  C��C��C��C��C�  C��3C��3C��3C��3C�  C��C��C�  C��3C�  C��3C��3C��3C�  C��C�  C�  C�  C�  C��3C��3C��3C�  C��C��C��C��3C�  C�  C��3C��C�  C��3C��C�  C�  C�  C��3C��C��3C��C��3C�  C��C��3C��C��C�  C�  C�  C��C�  C�  C��3C��C�  C�  C�  C�  C�  C�  C��3C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C��C�  C�  C��C�  C��C�  D   D � D  Dy�D  Dy�DfD�fDfDy�D��D�fD  D��DfD� D  D� D	  D	� D
  D
�fDfDy�D��Dy�DfD�fDfD��D  D�fD��D�fDfDy�D��D�fDfDy�D  D� D��D� D  D� D  Dy�DfD� D�3Dy�D��D�fD��D�fD��D� D��D� D  D�fD��D� D fD �fD ��D!� D"  D"� D#  D#� D$  D$y�D%fD%� D%��D&� D'fD'� D(fD(y�D)  D)� D*  D*� D*��D+� D,fD,y�D-fD-�fD-��D.��D/  D/� D0fD0� D0��D1�fD1��D2�fD3  D3�fD4  D4� D5fD5y�D6  D6y�D7fD7y�D8  D8�fD8��D9�fD:  D:y�D;  D;y�D<  D<� D=  D=y�D>fD>y�D?  D?� D@  D@� DA  DA� DB  DBy�DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DM��DN� DO  DO� DP  DP� DQ  DQ� DR  DR�fDR��DS� DT  DT� DUfDU� DV  DV� DV��DW� DW��DX� DYfDY� DY��DZ� D[fD[y�D\  D\� D\��D]� D^  D^�fD^��D_� D`fD`y�Da  Da��Db  Db� DcfDc� Dc��Ddy�De  De�fDffDf�fDg�Dg�fDg��Dh�fDi  Diy�DjfDj� DkfDk� Dk��Dl� DmfDm� Dm��Dn� DofDo� Do��Dpy�Dp��Dq� Dr  Dr�fDs  Ds� Dt  Dt� Du  Du� Dv  Dv� DwfDw�fDw��Dy��D�4)D��q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�=qA�A!�AA�Aa�A��\A��\A��\A��\A��\AЏ\A��\A��\B G�BG�BG�BG�B G�B(G�B0G�B8G�B@G�BHG�BPG�BXG�B`G�BhG�Bp�Bx�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B��B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B��B�#�B�#�B��B�#�C EC�RC�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Cg�RCj�Cl�Cm�RCo�RCq�RCt�Cv+�Cx�Cz�C|+�C~�C�RC��)C��C��C��)C��)C��C��C��)C��)C��)C��C��C��C��C��C��C��C��C��)C��C��C��C��)C��)C��C��C��C��C��C��C��C��C��C��)C��)C��)C��)C��C��C��C��C��)C��C��)C��)C��)C��C��C��C��C��C��C��)C��)C��)C��C��C��C�"�C��)C��C��C��)C��C��C��)C��C��C��C��C��)C��C��)C��C��)C��C��C��)C��C��C��C��C��C��C��C��C��)C��C��C��C��C��C��C��C��)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��)C��C��C��C��C��C��C��C��C��C��C��C��C��D {D �{D{D~D{D~D
�D��D
�D~D�D��D{D�HD
�D�{D{D�{D	{D	�{D
{D
��D
�D~D�D~D
�D��D
�D�HD{D��D�D��D
�D~D�D��D
�D~D{D�{D�D�{D{D�{D{D~D
�D�{D��D~D�D��D�D��D�D�{D�D�{D{D��D�D�{D 
�D ��D �D!�{D"{D"�{D#{D#�{D${D$~D%
�D%�{D%�D&�{D'
�D'�{D(
�D(~D){D)�{D*{D*�{D*�D+�{D,
�D,~D-
�D-��D-�D.�HD/{D/�{D0
�D0�{D0�D1��D1�D2��D3{D3��D4{D4�{D5
�D5~D6{D6~D7
�D7~D8{D8��D8�D9��D:{D:~D;{D;~D<{D<�{D={D=~D>
�D>~D?{D?�{D@{D@�{DA{DA�{DB{DB~DC{DC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DM�DN�{DO{DO�{DP{DP�{DQ{DQ�{DR{DR��DR�DS�{DT{DT�{DU
�DU�{DV{DV�{DV�DW�{DW�DX�{DY
�DY�{DY�DZ�{D[
�D[~D\{D\�{D\�D]�{D^{D^��D^�D_�{D`
�D`~Da{Da�HDb{Db�{Dc
�Dc�{Dc�Dd~De{De��Df
�Df��DgHDg��Dg�Dh��Di{Di~Dj
�Dj�{Dk
�Dk�{Dk�Dl�{Dm
�Dm�{Dm�Dn�{Do
�Do�{Do�Dp~Dp�Dq�{Dr{Dr��Ds{Ds�{Dt{Dt�{Du{Du�{Dv{Dv�{Dw
�Dw��Dw�Dy�D�6fD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�hsA�ffA�ffA�bNA�`BA�^5A�`BA�bNA�bNA�bNA�bNA�dZA�dZA�dZA�ffA�ffA�ffA�ffA�ffA�hsA�hsA�hsA�hsA�hsA�jA�jA�l�A�l�A�n�A�p�A�r�A�t�A�x�A̕�A�9XA�?}A�$�A���A��#AʮA�ffAɶFAɇ+A�l�A�7LA��A�ƨAȮAȕ�A���A��A�ffA�S�A�C�A�1'A� �A�1A���A�ZA�ĜA�v�A�bA�K�A�hsA��A�JA��jA�hsA���A�|�A��
A�M�A�A���A�ffA�O�A��A�x�A�+A���A���A�JA�1'A���A���A���A�I�A�hsA�v�A��A�t�A��
A�$�A���A���A��A�;dA�|�A��\A�|�A��jA�hsA�  A�oA�A�A�v�A���A���A�"�A��;A�?}A�A�A��#A�G�A�jA�bNA�5?A�l�A�O�A���A�$�A{�#Ay"�Av�`At�Aq�Ap�uAoVAl��Ail�Ag�TAe|�AbȴAaK�A_�A^bA]dZAXM�AV{AT$�ARQ�AP5?AO�FAM��AI�#AF��AD~�AA��A?hsA=K�A;S�A:�RA9�;A8�!A7p�A6(�A4��A3p�A2Q�A1�7A0�\A/��A/��A.�/A-;dA,9XA*��A)��A(�/A'��A%��A$ffA#��A#�PA ��A��A�HA�-A�+AZA~�A"�A�yA��A$�A�jA��A�`AQ�A�#A�/A^5AAVA�TA+A
��A
Q�A	A	��A	�AVA��A?}A�Av�A=qA�^A;dA��A=qA��A��A(�A�A33A �j@���@��@�dZ@�M�@�G�@��@� �@���@�o@��R@��+@��T@��@��@�9X@�v�@��@�bN@�@��@�-@��@�@�"�@�^@��/@���@�R@�~�@�@�r�@�\)@ݩ�@�r�@�ƨ@�K�@�@��@���@�V@�/@Ӆ@��H@Ұ!@���@�S�@�X@ɩ�@�z�@��@���@�-@ũ�@�1'@Õ�@�o@��@��@��@���@�@�5?@��@�bN@�S�@���@��@���@��+@���@���@��w@�
=@�E�@��#@���@��@�O�@���@���@�(�@�\)@���@��@��^@��@�G�@��j@��@��@��R@�^5@�$�@�{@��#@�p�@�V@��@� �@���@�K�@�
=@�
=@��@��@��+@���@�/@��D@�bN@��w@��@�;d@���@��@���@�S�@��@�"�@�5?@�?}@���@�z�@�ƨ@�\)@��!@��@�x�@��@��9@���@�j@�1'@��m@���@��P@�\)@�+@��H@��!@��+@�^5@�5?@��@��-@�V@��@��D@�(�@�(�@�9X@�1'@�1'@� �@��w@�K�@�-@��-@���@��7@�hs@��@��D@��D@��D@��@���@��@�+@���@�ff@�V@�E�@��@��-@��@��`@��D@�z�@��@���@���@��w@��@�|�@��P@��@��@�t�@�l�@�C�@�o@��@��R@��!@�~�@�ff@�n�@�{@���@�7L@��/@���@�Ĝ@��9@�r�@�Z@�A�@�1'@� �@�b@��w@���@���@��@�33@���@�M�@��T@�O�@���@�9X@�|�@���@�-@�J@��@���@�X@��/@��j@���@��D@��@�r�@�Q�@�A�@�1'@��@��
@���@�l�@��@��+@�M�@�$�@���@��#@�hs@��`@��/@��j@��u@��D@��@�r�@�j@�  @��;@��
@�ƨ@���@���@��@�;d@���@�ȴ@��+@�^5@�$�@��T@��h@�/@���@�bN@�Z@�r�@�bN@��	@v�@c$t111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�hsA�ffA�ffA�bNA�`BA�^5A�`BA�bNA�bNA�bNA�bNA�dZA�dZA�dZA�ffA�ffA�ffA�ffA�ffA�hsA�hsA�hsA�hsA�hsA�jA�jA�l�A�l�A�n�A�p�A�r�A�t�A�x�A̕�A�9XA�?}A�$�A���A��#AʮA�ffAɶFAɇ+A�l�A�7LA��A�ƨAȮAȕ�A���A��A�ffA�S�A�C�A�1'A� �A�1A���A�ZA�ĜA�v�A�bA�K�A�hsA��A�JA��jA�hsA���A�|�A��
A�M�A�A���A�ffA�O�A��A�x�A�+A���A���A�JA�1'A���A���A���A�I�A�hsA�v�A��A�t�A��
A�$�A���A���A��A�;dA�|�A��\A�|�A��jA�hsA�  A�oA�A�A�v�A���A���A�"�A��;A�?}A�A�A��#A�G�A�jA�bNA�5?A�l�A�O�A���A�$�A{�#Ay"�Av�`At�Aq�Ap�uAoVAl��Ail�Ag�TAe|�AbȴAaK�A_�A^bA]dZAXM�AV{AT$�ARQ�AP5?AO�FAM��AI�#AF��AD~�AA��A?hsA=K�A;S�A:�RA9�;A8�!A7p�A6(�A4��A3p�A2Q�A1�7A0�\A/��A/��A.�/A-;dA,9XA*��A)��A(�/A'��A%��A$ffA#��A#�PA ��A��A�HA�-A�+AZA~�A"�A�yA��A$�A�jA��A�`AQ�A�#A�/A^5AAVA�TA+A
��A
Q�A	A	��A	�AVA��A?}A�Av�A=qA�^A;dA��A=qA��A��A(�A�A33A �j@���@��@�dZ@�M�@�G�@��@� �@���@�o@��R@��+@��T@��@��@�9X@�v�@��@�bN@�@��@�-@��@�@�"�@�^@��/@���@�R@�~�@�@�r�@�\)@ݩ�@�r�@�ƨ@�K�@�@��@���@�V@�/@Ӆ@��H@Ұ!@���@�S�@�X@ɩ�@�z�@��@���@�-@ũ�@�1'@Õ�@�o@��@��@��@���@�@�5?@��@�bN@�S�@���@��@���@��+@���@���@��w@�
=@�E�@��#@���@��@�O�@���@���@�(�@�\)@���@��@��^@��@�G�@��j@��@��@��R@�^5@�$�@�{@��#@�p�@�V@��@� �@���@�K�@�
=@�
=@��@��@��+@���@�/@��D@�bN@��w@��@�;d@���@��@���@�S�@��@�"�@�5?@�?}@���@�z�@�ƨ@�\)@��!@��@�x�@��@��9@���@�j@�1'@��m@���@��P@�\)@�+@��H@��!@��+@�^5@�5?@��@��-@�V@��@��D@�(�@�(�@�9X@�1'@�1'@� �@��w@�K�@�-@��-@���@��7@�hs@��@��D@��D@��D@��@���@��@�+@���@�ff@�V@�E�@��@��-@��@��`@��D@�z�@��@���@���@��w@��@�|�@��P@��@��@�t�@�l�@�C�@�o@��@��R@��!@�~�@�ff@�n�@�{@���@�7L@��/@���@�Ĝ@��9@�r�@�Z@�A�@�1'@� �@�b@��w@���@���@��@�33@���@�M�@��T@�O�@���@�9X@�|�@���@�-@�J@��@���@�X@��/@��j@���@��D@��@�r�@�Q�@�A�@�1'@��@��
@���@�l�@��@��+@�M�@�$�@���@��#@�hs@��`@��/@��j@��u@��D@��@�r�@�j@�  @��;@��
@�ƨ@���@���@��@�;d@���@�ȴ@��+@�^5@�$�@��T@��h@�/@���@�bN@�Z@�r�@�bN@��	@v�@c$t111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
5?B
5?B
5?B
6FB
5?B
6FB
6FB
7LB
6FB
7LB
7LB
9XB
;dB
<jB
>wB
C�B
�B
B
�RB
��B
��B
��B
��B
��B
ÖB
ɺB
�
B
�BB
�B
��BJBbB(�BN�Bo�Bq�Br�Bq�Bp�Bn�B�1B��B�5BBbB�B(�B>wBD�BE�BE�BD�BA�BA�BB�BI�BN�BE�BC�BG�BR�BS�BVBR�BK�B;dB(�BB��B�B�NB�
B��BÖB�wB�FB�B��B��B�\B�Bv�BhsB_;BZBR�BG�B=qB33B,B$�B�BoBB
�B
��B
�?B
��B
��B
�JB
q�B
S�B
G�B
<jB
�B
+B	�B	�HB	��B	ȴB	�jB	��B	�hB	�B	r�B	aHB	W
B	N�B	E�B	?}B	(�B	�B	bB	+B��B��B�B�BB��BȴB��B�LB�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�JB�+B�B�B�B� B}�Bz�Bw�By�Bw�Bw�Bw�Bv�Bx�Bw�Bw�Bx�Bw�Bt�Bv�Bv�Bv�B|�B� B~�B|�B}�B}�B�B�B�%B�7B�7B�DB�JB�JB�VB�\B�bB�\B�\B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�!B�'B�9B�?B�LB�LB�FB�FB�B�B�B�B�B�B�!B�!B�!B�-B�9B�9B�?B�LB�XB�^B�^B�jB�wB�wBĜB��B�wB�}BBÖBǮB��B��B�B�#B�BB�mB�sB�yB�B�B�B��B��B	B	B	B	%B	+B	DB	\B	{B	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	$�B	'�B	)�B	)�B	,B	,B	/B	7LB	=qB	@�B	B�B	C�B	D�B	F�B	L�B	N�B	O�B	R�B	T�B	VB	W
B	YB	ZB	ZB	\)B	^5B	^5B	^5B	_;B	]/B	^5B	dZB	hsB	jB	k�B	l�B	o�B	r�B	s�B	u�B	y�B	z�B	}�B	}�B	|�B	}�B	�B	� B	� B	�B	�B	�B	�%B	�7B	�DB	�DB	�JB	�VB	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�?B	�FB	�RB	�dB	�dB	�^B	�jB	�wB	��B	��B	��B	ÖB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�#B	�#B	�)B	�/B	�/B	�/B	�/B	�;B	�NB	�ZB	�fB	�mB	�sB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�sB	�sB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
1B

=B
DB
JB
JB
DB
DB
JB
VB
\B
�B
"B
0U222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
5?B
5?B
5?B
6FB
5?B
6FB
6FB
7LB
6FB
7LB
7LB
9XB
;dB
<jB
>wB
C�B
�B
B
�RB
��B
��B
��B
��B
��B
ÖB
ɺB
�
B
�BB
�B
��BJBbB(�BN�Bo�Bq�Br�Bq�Bp�Bn�B�1B��B�5BBbB�B(�B>wBD�BE�BE�BD�BA�BA�BB�BI�BN�BE�BC�BG�BR�BS�BVBR�BK�B;dB(�BB��B�B�NB�
B��BÖB�wB�FB�B��B��B�\B�Bv�BhsB_;BZBR�BG�B=qB33B,B$�B�BoBB
�B
��B
�?B
��B
��B
�JB
q�B
S�B
G�B
<jB
�B
+B	�B	�HB	��B	ȴB	�jB	��B	�hB	�B	r�B	aHB	W
B	N�B	E�B	?}B	(�B	�B	bB	+B��B��B�B�BB��BȴB��B�LB�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�JB�+B�B�B�B� B}�Bz�Bw�By�Bw�Bw�Bw�Bv�Bx�Bw�Bw�Bx�Bw�Bt�Bv�Bv�Bv�B|�B� B~�B|�B}�B}�B�B�B�%B�7B�7B�DB�JB�JB�VB�\B�bB�\B�\B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�!B�'B�9B�?B�LB�LB�FB�FB�B�B�B�B�B�B�!B�!B�!B�-B�9B�9B�?B�LB�XB�^B�^B�jB�wB�wBĜB��B�wB�}BBÖBǮB��B��B�B�#B�BB�mB�sB�yB�B�B�B��B��B	B	B	B	%B	+B	DB	\B	{B	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	$�B	'�B	)�B	)�B	,B	,B	/B	7LB	=qB	@�B	B�B	C�B	D�B	F�B	L�B	N�B	O�B	R�B	T�B	VB	W
B	YB	ZB	ZB	\)B	^5B	^5B	^5B	_;B	]/B	^5B	dZB	hsB	jB	k�B	l�B	o�B	r�B	s�B	u�B	y�B	z�B	}�B	}�B	|�B	}�B	�B	� B	� B	�B	�B	�B	�%B	�7B	�DB	�DB	�JB	�VB	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�?B	�FB	�RB	�dB	�dB	�^B	�jB	�wB	��B	��B	��B	ÖB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�#B	�#B	�)B	�/B	�/B	�/B	�/B	�;B	�NB	�ZB	�fB	�mB	�sB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�sB	�sB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
1B

=B
DB
JB
JB
DB
DB
JB
VB
\B
�B
"B
0U222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.07 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191651                              AO  ARCAADJP                                                                    20181005191651    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191651  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191651  QCF$                G�O�G�O�G�O�8000            