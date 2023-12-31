CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:53Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191753  20181005191753  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��d�r1   @��eww�0@5Tz�G��d~-V1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @��@�  @�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�fC�fC  C
  C  C  C  C  C�fC�fC  C  C  C  C   C"  C$  C&  C(  C*�C,�C.  C0  C2  C4  C5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CY�fC\  C^  C`�Cb�Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cw�fCz  C|  C~  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C��3C��3C��3C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C�  C�  C��3C��3C�  C�  C��C�  C��3C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C��C��C�  C�  C��3C�  C��C�  C�  C��D   D y�D  D� D  D� D  D�fDfD� D��D�fD  Dy�DfD� D��D� D	  D	� D
  D
�fD  Dy�D��Dy�DfD� D��D�fDfD�fD  D� D  Dy�D  D�fD  Dy�D��Dy�D��Dy�D��Dy�D��Dy�DfD�fD  D�fD  D� D  D� D��Ds3D��D� D  D�fD  Dy�D   D �fD!fD!�fD"fD"� D"��D#� D$  D$� D%fD%� D&  D&�fD'fD'�fD(fD(� D)  D)� D*fD*�fD+  D+y�D+��D,y�D-  D-�fD.  D.y�D/  D/� D0fD0��D1  D1y�D2  D2y�D3  D3� D4  D4y�D4��D5�fD6  D6� D7  D7� D8  D8y�D9  D9�fD:fD:� D;  D;�fD<  D<� D<��D=y�D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC�fDDfDD� DE  DE� DFfDF� DF��DGy�DG��DH� DI  DI�fDJ  DJ�fDK  DKy�DLfDL� DL��DM� DM��DN� DN��DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DUy�DV  DV� DW  DWy�DX  DX� DX��DY� DZ  DZy�D[  D[�fD\  D\� D]fD]� D^  D^� D_  D_y�D`  D`� Da  Da� DbfDb� Db��Dcy�Dd  Dd�fDefDe�fDffDf�fDgfDg� Dg��Dh�fDi  Diy�Di��Djy�Dk  Dk�fDl  Dly�Dm  Dm�fDn  Dny�DofDo� Do��Dp�fDq  Dq� Dr  Dr� DsfDs� DtfDt� Du  Duy�Dv  Dv� Dv��Dwy�Dyz=D�.�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @.�R@��\@ʏ\AG�A%G�AEG�Ac�A���A���A���A���A£�Aң�A��A��BQ�B	Q�BQ�BQ�B!Q�B)Q�B1Q�B9Q�BAQ�BIQ�BQQ�BYQ�BaQ�BiQ�BqQ�ByQ�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B��)B���BĨ�BȨ�B̨�B��)BԨ�Bب�Bܨ�B��B��B��B��B��B���B���B���C T{CT{C:�C:�CT{C
T{CT{CT{CT{CT{C:�C:�CT{CT{CT{CT{C T{C"T{C$T{C&T{C(T{C*nC,nC.T{C0T{C2T{C4T{C6:�C8T{C:T{C<T{C>T{C@T{CBT{CDT{CFT{CHT{CJT{CLT{CNT{CPT{CRT{CTT{CVT{CXT{CZ:�C\T{C^T{C`nCbnCdnCfT{ChT{CjT{ClT{CnT{CpT{CrT{CtT{CvT{Cx:�CzT{C|T{C~T{C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�7
C�7
C�7
C�*=C�*=C�*=C�7
C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�pC�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�pC�pC�*=C�*=C�pC�pC�pC�*=C�7
C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�pC�pC�*=C�*=C�pC�pC�*=C�*=C�7
C�*=C�pC�*=C�pC�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�7
C�*=C�*=C�*=C�*=C�7
C�7
C�*=C�*=C�pC�*=C�7
C�*=C�*=C�7
D D ��DD�DD�DD��D�D�D�D��DD��D�D�D�D�D	D	�D
D
��DD��D�D��D�D�D�D��D�D��DD�DD��DD��DD��D�D��D�D��D�D��D�D��D�D��DD��DD�DD�D�D�RD�D�DD��DD��D D ��D!�D!��D"�D"�D#�D#�D$D$�D%�D%�D&D&��D'�D'��D(�D(�D)D)�D*�D*��D+D+��D,�D,��D-D-��D.D.��D/D/�D0�D0��D1D1��D2D2��D3D3�D4D4��D5�D5��D6D6�D7D7�D8D8��D9D9��D:�D:�D;D;��D<D<�D=�D=��D>D>�D?D?�D@D@�DADA�DBDB�DCDC��DD�DD�DEDE�DF�DF�DG�DG��DH�DH�DIDI��DJDJ��DKDK��DL�DL�DM�DM�DN�DN�DO�DO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU��DVDV�DWDW��DXDX�DY�DY�DZDZ��D[D[��D\D\�D]�D]�D^D^�D_D_��D`D`�DaDa�Db�Db�Dc�Dc��DdDd��De�De��Df�Df��Dg�Dg�Dh�Dh��DiDi��Dj�Dj��DkDk��DlDl��DmDm��DnDn��Do�Do�Dp�Dp��DqDq�DrDr�Ds�Ds�Dt�Dt�DuDu��DvDv�Dw�Dw��Dy�\D�9GD�˅111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��A��A�"�A��A� �A�"�A� �A��A���A�ȴA�bNA��A��wA�ffA�/A��A�A�A��jA��^A��A���A��7A�r�A�ZA�G�A�C�A�1'A��A��A��A�{A��A�bA�oA��A��A��A�{A�oA��A�&�A�"�A��A��A��A�{A�{A�{A�bA�bA�bA�bA�
=A�%A���A��mA���A�A�A�p�A�A��`A���A�$�A���A�n�A���A�A�A�&�A�ZA�/A�33A��A�Q�A��;A�$�A��DA�VA�K�A�7LA�(�A�r�A�1'A��FA�ZA���A��RA���A���A�A��^A��A��-A�M�A�ȴA�^5A�1'A��yA��+A�A�A���A��A�A�r�A�ĜA���A�VA��RA�"�A�1A�v�A���A�n�A���A��A�1'A��;A|�\A|z�Az��Ax�+AuAt�9AsK�Aq&�ApVAn�Al�Aj�Af�\Ad��Aa��A^ȴA^$�A^(�A\jA[��A[x�AZ�uAX �AW`BAT��AT��ATZAS�^AR�!ARI�AR=qAQ�PAP�+AO��ANn�AL��AL$�AJ��AI�#AH��AG�;AGVAEt�AD�\ACACoAA��AAl�A@A>�A<�A:��A8{A7l�A733A6��A6ffA5�A2�A0��A/��A/�hA/
=A.�!A-��A,$�A)�A);dA'��A&~�A&(�A%�wA%K�A%VA$�A#�A#"�A"9XA!O�A �DAA��Al�A�A��AM�A��A�/AI�A�A�AE�A�HA��A/A�7A~�A�#A��A&�AA�FA��A��A��A
�HA
��A
5?A	�7A	?}A�uAƨA~�AXA�RAA�AhsA~�A��@��@�@��9@���@�1@��^@�@�@�j@�w@�l�@�C�@�M�@�x�@�%@�1'@�S�@�\@�@�(�@�-@�7L@��@�t�@�\)@�;d@◍@��@���@���@��@�-@�1@ە�@��@���@�@��
@���@թ�@�(�@�;d@Ѻ^@�ƨ@�"�@ͺ^@̼j@�A�@�dZ@���@ʇ+@�E�@�@ȴ9@�|�@�~�@��/@ă@�A�@���@§�@��^@���@�j@�b@�l�@���@�n�@�$�@���@�V@�A�@���@�33@�~�@�p�@���@���@�K�@�ȴ@�v�@��@�`B@���@� �@��@�hs@���@�;d@�ȴ@��R@��!@�{@��@�/@��@���@�t�@��!@�~�@�$�@���@�x�@�`B@�G�@�?}@�&�@���@��u@�(�@��@�ƨ@�\)@�o@�ȴ@��R@�ff@�=q@��@�`B@��@�%@���@��9@�Z@�A�@�b@�l�@��R@���@���@�^5@��@���@�hs@�X@�?}@��@���@�  @�K�@��!@�M�@�-@�v�@�~�@�~�@�M�@�M�@�E�@�E�@�{@���@���@�x�@�V@�r�@��@���@�l�@��y@�E�@���@���@�X@���@�I�@��;@��P@��@��@��H@��H@���@�V@�5?@�@��#@���@�p�@�/@���@��/@��u@�z�@�r�@�bN@�A�@�b@��m@��w@���@��@�dZ@�;d@�
=@���@�5?@��@�{@���@�hs@�/@��@��@��j@���@��D@�Z@� �@��w@��@�S�@�;d@�
=@��!@��+@�ff@�E�@��T@��h@�p�@�?}@�/@��@�%@���@��@��`@���@��j@���@��D@�I�@�  @���@��m@��F@��
@��
@���@���@��+@�^5@�V@�E�@�{@�J@���@�@�`B@��@��@�Z@� �@���@���@�/�@|�_@i��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A��A�"�A��A� �A�"�A� �A��A���A�ȴA�bNA��A��wA�ffA�/A��A�A�A��jA��^A��A���A��7A�r�A�ZA�G�A�C�A�1'A��A��A��A�{A��A�bA�oA��A��A��A�{A�oA��A�&�A�"�A��A��A��A�{A�{A�{A�bA�bA�bA�bA�
=A�%A���A��mA���A�A�A�p�A�A��`A���A�$�A���A�n�A���A�A�A�&�A�ZA�/A�33A��A�Q�A��;A�$�A��DA�VA�K�A�7LA�(�A�r�A�1'A��FA�ZA���A��RA���A���A�A��^A��A��-A�M�A�ȴA�^5A�1'A��yA��+A�A�A���A��A�A�r�A�ĜA���A�VA��RA�"�A�1A�v�A���A�n�A���A��A�1'A��;A|�\A|z�Az��Ax�+AuAt�9AsK�Aq&�ApVAn�Al�Aj�Af�\Ad��Aa��A^ȴA^$�A^(�A\jA[��A[x�AZ�uAX �AW`BAT��AT��ATZAS�^AR�!ARI�AR=qAQ�PAP�+AO��ANn�AL��AL$�AJ��AI�#AH��AG�;AGVAEt�AD�\ACACoAA��AAl�A@A>�A<�A:��A8{A7l�A733A6��A6ffA5�A2�A0��A/��A/�hA/
=A.�!A-��A,$�A)�A);dA'��A&~�A&(�A%�wA%K�A%VA$�A#�A#"�A"9XA!O�A �DAA��Al�A�A��AM�A��A�/AI�A�A�AE�A�HA��A/A�7A~�A�#A��A&�AA�FA��A��A��A
�HA
��A
5?A	�7A	?}A�uAƨA~�AXA�RAA�AhsA~�A��@��@�@��9@���@�1@��^@�@�@�j@�w@�l�@�C�@�M�@�x�@�%@�1'@�S�@�\@�@�(�@�-@�7L@��@�t�@�\)@�;d@◍@��@���@���@��@�-@�1@ە�@��@���@�@��
@���@թ�@�(�@�;d@Ѻ^@�ƨ@�"�@ͺ^@̼j@�A�@�dZ@���@ʇ+@�E�@�@ȴ9@�|�@�~�@��/@ă@�A�@���@§�@��^@���@�j@�b@�l�@���@�n�@�$�@���@�V@�A�@���@�33@�~�@�p�@���@���@�K�@�ȴ@�v�@��@�`B@���@� �@��@�hs@���@�;d@�ȴ@��R@��!@�{@��@�/@��@���@�t�@��!@�~�@�$�@���@�x�@�`B@�G�@�?}@�&�@���@��u@�(�@��@�ƨ@�\)@�o@�ȴ@��R@�ff@�=q@��@�`B@��@�%@���@��9@�Z@�A�@�b@�l�@��R@���@���@�^5@��@���@�hs@�X@�?}@��@���@�  @�K�@��!@�M�@�-@�v�@�~�@�~�@�M�@�M�@�E�@�E�@�{@���@���@�x�@�V@�r�@��@���@�l�@��y@�E�@���@���@�X@���@�I�@��;@��P@��@��@��H@��H@���@�V@�5?@�@��#@���@�p�@�/@���@��/@��u@�z�@�r�@�bN@�A�@�b@��m@��w@���@��@�dZ@�;d@�
=@���@�5?@��@�{@���@�hs@�/@��@��@��j@���@��D@�Z@� �@��w@��@�S�@�;d@�
=@��!@��+@�ff@�E�@��T@��h@�p�@�?}@�/@��@�%@���@��@��`@���@��j@���@��D@�I�@�  @���@��m@��F@��
@��
@���@���@��+@�^5@�V@�E�@�{@�J@���@�@�`B@��@��@�Z@� �@���@���@�/�@|�_@i��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BM�BM�BM�BM�BM�BM�BM�BL�BL�BM�BN�BP�BYB_;BffBo�Bw�B�B�7B�PB�hB�oB�hB�hB�hB�VB�DB�=B�7B�+B�%B�+B�+B�+B�7B�1B�7B�=B�=B�DB�=B�=B�VB�hB�oB�oB�oB�uB�uB�uB�uB�uB�uB�uB�{B�uB�uB�{B�uB��B��B��B��B��B��B��B��B��B�B��B��B��B�VB~�Bw�Bs�Bp�Bl�BgmBdZBZBJ�B8RB0!B/B'�B"�B�B{B	7B��B�yB�;B��BɺB�LB��B�VB|�Bv�Bp�Bk�BcTBYBJ�BC�B8RB)�B�BPB%B1BB
��B
��B
��B
�`B
��B
��B
v�B
v�B
bNB
G�B
&�B
�B
PB	��B	�B	�NB	��B	�qB	��B	�\B	y�B	hsB	� B	��B	�\B	�%B	�B	z�B	iyB	aHB	N�B	O�B	T�B	Q�B	F�B	D�B	G�B	I�B	C�B	@�B	<jB	6FB	2-B	.B	+B	'�B	%�B	!�B	�B	�B	�B	�B	hB	VB		7B	B��B��B�B�B�B�yB�fB�HB�B��B��B��B��BɺBŢB�}B�LB�3B�B��B��B��B��B��B��B��B��B��B��B��B�uB�\B�JB�=B�7B�+B�%B�B�B�B� B}�B{�Bz�By�Bx�B{�Bz�By�Bx�Bx�Bx�Bw�Bv�Bv�Bv�Bu�Bu�Bu�Bt�Bs�Br�Br�Bq�Bq�Bp�Bo�Bn�Bl�Bm�Bk�BjBiyBgmBhsBffBgmBgmBffBffBffBgmBgmBgmBgmBgmBhsBiyBhsBiyBl�Bn�Bn�Bn�Bn�Bp�Bu�Bw�By�Bz�By�B~�B� B�B�B�B�B�%B�7B�PB�\B�oB��B��B��B��B��B��B��B��B��B��B��B�B�B�9B�?B�?B�FB�dB�wBBĜBƨBȴB��B��B��B��B��B�
B�B�)B�BB�fB�yB�yB�B��B��B��B��B	B	1B	uB	uB	�B	�B	�B	�B	�B	 �B	"�B	#�B	%�B	'�B	)�B	.B	0!B	2-B	5?B	9XB	:^B	:^B	;dB	<jB	=qB	>wB	@�B	@�B	C�B	F�B	J�B	L�B	L�B	O�B	P�B	P�B	VB	XB	YB	^5B	`BB	bNB	cTB	cTB	gmB	l�B	l�B	l�B	m�B	n�B	p�B	u�B	y�B	|�B	�B	�B	�B	�B	�%B	�1B	�JB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�3B	�3B	�9B	�?B	�?B	�?B	�9B	�?B	�?B	�FB	�LB	�RB	�RB	�XB	�^B	�dB	�qB	�}B	��B	B	ĜB	ĜB	ĜB	ŢB	ŢB	ƨB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�5B	�5B	�BB	�HB	�HB	�NB	�ZB	�ZB	�mB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
%B
%B
B
B
1B
	7B
�B
-�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  BM�BM�BM�BM�BM�BM�BM�BL�BL�BM�BN�BP�BYB_;BffBo�Bw�B�B�7B�PB�hB�oB�hB�hB�hB�VB�DB�=B�7B�+B�%B�+B�+B�+B�7B�1B�7B�=B�=B�DB�=B�=B�VB�hB�oB�oB�oB�uB�uB�uB�uB�uB�uB�uB�{B�uB�uB�{B�uB��B��B��B��B��B��B��B��B��B�B��B��B��B�VB~�Bw�Bs�Bp�Bl�BgmBdZBZBJ�B8RB0!B/B'�B"�B�B{B	7B��B�yB�;B��BɺB�LB��B�VB|�Bv�Bp�Bk�BcTBYBJ�BC�B8RB)�B�BPB%B1BB
��B
��B
��B
�`B
��B
��B
v�B
v�B
bNB
G�B
&�B
�B
PB	��B	�B	�NB	��B	�qB	��B	�\B	y�B	hsB	� B	��B	�\B	�%B	�B	z�B	iyB	aHB	N�B	O�B	T�B	Q�B	F�B	D�B	G�B	I�B	C�B	@�B	<jB	6FB	2-B	.B	+B	'�B	%�B	!�B	�B	�B	�B	�B	hB	VB		7B	B��B��B�B�B�B�yB�fB�HB�B��B��B��B��BɺBŢB�}B�LB�3B�B��B��B��B��B��B��B��B��B��B��B��B�uB�\B�JB�=B�7B�+B�%B�B�B�B� B}�B{�Bz�By�Bx�B{�Bz�By�Bx�Bx�Bx�Bw�Bv�Bv�Bv�Bu�Bu�Bu�Bt�Bs�Br�Br�Bq�Bq�Bp�Bo�Bn�Bl�Bm�Bk�BjBiyBgmBhsBffBgmBgmBffBffBffBgmBgmBgmBgmBgmBhsBiyBhsBiyBl�Bn�Bn�Bn�Bn�Bp�Bu�Bw�By�Bz�By�B~�B� B�B�B�B�B�%B�7B�PB�\B�oB��B��B��B��B��B��B��B��B��B��B��B�B�B�9B�?B�?B�FB�dB�wBBĜBƨBȴB��B��B��B��B��B�
B�B�)B�BB�fB�yB�yB�B��B��B��B��B	B	1B	uB	uB	�B	�B	�B	�B	�B	 �B	"�B	#�B	%�B	'�B	)�B	.B	0!B	2-B	5?B	9XB	:^B	:^B	;dB	<jB	=qB	>wB	@�B	@�B	C�B	F�B	J�B	L�B	L�B	O�B	P�B	P�B	VB	XB	YB	^5B	`BB	bNB	cTB	cTB	gmB	l�B	l�B	l�B	m�B	n�B	p�B	u�B	y�B	|�B	�B	�B	�B	�B	�%B	�1B	�JB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�3B	�3B	�9B	�?B	�?B	�?B	�9B	�?B	�?B	�FB	�LB	�RB	�RB	�XB	�^B	�dB	�qB	�}B	��B	B	ĜB	ĜB	ĜB	ŢB	ŢB	ƨB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�5B	�5B	�BB	�HB	�HB	�NB	�ZB	�ZB	�mB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
%B
%B
B
B
1B
	7B
�B
-�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.33 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191753                              AO  ARCAADJP                                                                    20181005191753    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191753  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191753  QCF$                G�O�G�O�G�O�8000            