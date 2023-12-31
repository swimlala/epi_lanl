CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:19Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005191719  20181005191719  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��d܊1   @��ehK��@5S����dv-V1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   B   B   @@  @�  @�  A   A   A@  A`  A�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBG��BO��BX  B`  Bh  Bp  Bx  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�33C   C  C�fC  C  C
  C  C�fC�fC  C  C  C  C�C  C�fC   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CS�fCV  CX  CZ  C[�fC]�fC_�fCb  Cd�Cf�Ch�Cj�Cl  Cn�Cp�Cr  Cs�fCv  Cw�fCz  C|  C~  C�  C��C��C��C��C�  C��C��C�  C�  C��C�  C�  C��C��C�  C�  C��C�  C�  C��C�  C��3C�  C��C�  C��3C��3C��3C��3C��3C��3C�  C�  C�  C��3C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C��C��3C��3C��C�  C��C��C��C�  C��C��C��3C�  C��C��C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C�  C��C�  C��3C��fC�  C�  C��C��C�  C�  C��C��C��C�  C��3C��3C��3C��3C�  C�  C�  C��3C��3C�  C�  C��C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��C�  C��3C�  C��C�  C�  C�  C�  D   D � DfD� D  D� D  D�fD  Dy�DfD� D  D�fDfD� DfD� D	  D	� D	��D
y�D  D� D��Dy�D��D� DfD� DfD� DfD�fD  D� DfD�fD  Dy�D��D� D  Dy�DfD�fD�D�fD  Dy�D��Dy�D��D� D  Dy�D��Ds3D��D� DfD�fDfD�fD fD � D ��D!� D"fD"�fD#  D#y�D#�3D$y�D$��D%y�D&  D&� D&��D'y�D'��D(� D)fD)�fD*  D*� D+fD+� D,  D,�fD-  D-y�D-��D.� D/  D/� D0  D0�fD1fD1�fD2fD2�fD3  D3� D4fD4�fD5  D5� D6  D6� D7  D7� D8fD8� D9  D9� D9��DF  DF�fDGfDG� DH  DH�fDI  DI� DJfDJ� DK  DK� DL  DL�fDMfDM�fDNfDN� DO  DO� DP  DP� DQ  DQ� DR  DR� DR��DSy�DT  DT�fDUfDU�fDVfDV� DV��DWy�DX  DX� DX��DY� DZ  DZy�D[  D[�fD[��D\y�D\��D]� D]��D^� D_fD_� D`  D`� D`��Day�Db  Db� Dc  Dc� Dd  Dd� De  Dey�De��Dfy�Df��Dgy�Dh  Dhy�Di  Di� Dj  Dj� Dk  Dk� DlfDl� Dm  Dm� Dn  Dn� DofDo� Dp  Dp� Dq  Dq� Dr  Dry�Ds  Ds� Dt  Dt� Du  Duy�Du��Dv� Dw  Dw� DwٚDy~D�<{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @U�@��\@ʏ\AG�A%G�AEG�AeG�A���A�p�A���A���A£�Aң�A��A��BQ�B	Q�BQ�BQ�B!Q�B)Q�B1Q�B9Q�BA�RBH�BP�BYQ�BaQ�BiQ�BqQ�ByQ�B��)B��)B���B���B���B���B���B���B���B���B���B��)B���B���B���B���B���BĨ�BȨ�B̨�BШ�BԨ�B��)B��)B��B��B��B��B��B���B���B��)C T{CT{C:�CT{CT{C
T{CT{C:�C:�CT{CT{CT{CT{CnCT{C:�C T{C"nC$T{C&T{C(T{C*T{C,T{C.T{C0T{C2T{C4T{C6T{C8T{C:T{C<T{C>T{C@T{CBT{CDT{CFT{CHT{CJT{CLT{CNT{CPT{CRT{CT:�CVT{CXT{CZT{C\:�C^:�C`:�CbT{CdnCfnChnCjnClT{CnnCpnCrT{Ct:�CvT{Cx:�CzT{C|T{C~T{C�*=C�7
C�7
C�7
C�7
C�*=C�7
C�7
C�*=C�*=C�7
C�*=C�*=C�7
C�7
C�*=C�*=C�7
C�*=C�*=C�7
C�*=C�pC�*=C�7
C�*=C�pC�pC�pC�pC�pC�pC�*=C�*=C�*=C�pC�7
C�*=C�*=C�*=C�*=C�pC�pC�*=C�*=C�*=C�*=C�*=C�pC�7
C�pC�pC�C�C�*=C�7
C�7
C�7
C�*=C�7
C�7
C�pC�*=C�7
C�7
C�*=C�*=C�*=C�*=C�pC�pC�pC�pC�*=C�*=C�*=C�7
C�*=C�pC��C�*=C�*=C�7
C�7
C�*=C�*=C�7
C�C�C�7
C�*=C�pC�pC�pC�pC�*=C�*=C�*=C�pC�pC�*=C�*=C�7
C�*=C�*=C�*=C�7
C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�C�C�C�C�*=C�*=C�7
C�*=C�pC�*=C�7
C�*=C�*=C�*=C�*=D D �D�D�DD�DD��DD��D�D�DD��D�D�D�D�D	D	�D
�D
��DD�D�D��D�D�D�D�D�D�D�D��DD�D�D��DD��D�D�DD��D�D��D!�D��DD��D�D��D�D�DD��D�D�RD�D�D�D��D�D��D �D �D!�D!�D"�D"��D#D#��D$RD$��D%�D%��D&D&�D'�D'��D(�D(�D)�D)��D*D*�D+�D+�D,D,��D-D-��D.�D.�D/D/�D0D0��D1�D1��D2�D2��D3D3�D4�D4��D5D5�D6D6�D7D7�D8�D8�D9D9�D:�DFDF��DG�DG�DHDH��DIDI�DJ�DJ�DKDK�DLDL��DM�DM��DN�DN�DODO�DPDP�DQDQ�DRDR�DS�DS��DTDT��DU�DU��DV�DV�DW�DW��DXDX�DY�DY�DZDZ��D[D[��D\�D\��D]�D]�D^�D^�D_�D_�D`D`�Da�Da��DbDb�DcDc�DdDd�DeDe��Df�Df��Dg�Dg��DhDh��DiDi�DjDj�DkDk�Dl�Dl�DmDm�DnDn�Do�Do�DpDp�DqDq�DrDr��DsDs�DtDt�DuDu��Dv�Dv�DwDw�Dw�Dy�3D�G
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A�  A�A�A�%A�A��;Aۛ�A�XA��A��A�M�Aӥ�A��;A��A�bNA���Aɥ�A���A�  A�~�A�1'A��DA�5?A���A��A�5?A�Q�A��A�oA���A���A��/A���A�"�A��#A�{A��TA�VA�hsA���A���A��`A���A�1'A���A�t�A�S�A�ffA��
A��A��A�r�A�(�A���A�;dA��wA���A�VA�r�A�oA���A��#A�$�A�bA�?}A���A�I�A���A��A��A���A�(�A�O�A��TA�t�A�~�A�A�5?A�VA��A~�\A}33Az��Ay�PAr�ApI�Aol�An1Al�Ai�-Ah�Ag�AehsAa�A^9XA\��A\VA[;dAY�AXȴAU�ATr�AS�FAS��AS7LAR5?AQC�AP��AO��AL��AK�FAJ�AI�7AG�7ADjAA��A?�
A>bA<Q�A:-A5�A4�RA3oA1��A/�mA/A-`BA+oA*�DA*E�A)��A'��A'�A$��A#�A!�A�A�`A��AI�A�TA�RA��AA|�A�`A=qA�TAA�A��AG�A�A�A�9AĜA
1'AȴA-Ap�AȴA �A��A�!A�A�A=qA �@�@�ff@��T@�X@�9X@�"�@�Z@�@�x�@�@�p�@�/@�@�o@�M�@�@�I�@ꟾ@��T@�ƨ@�+@���@� �@��
@��@�@�A�@��@��T@ݑh@�&�@܃@ۥ�@ڗ�@���@ם�@�&�@�K�@ҟ�@Ѳ-@�x�@�Q�@��@�-@̴9@�\)@�@ȋD@�l�@��y@Ƈ+@�E�@���@őh@�/@�/@��@�(�@+@��@�Q�@��m@��@�l�@�K�@�;d@��@��u@�@��\@�5?@���@�1'@��H@�  @�/@���@�S�@�o@���@��\@��@��m@�\)@�+@�"�@��@�@��!@�n�@�ff@�5?@��h@�/@��@��D@�1@���@���@�5?@�-@��@�O�@��@���@��@�z�@� �@���@�|�@�|�@��F@�dZ@�t�@��@��F@��@���@���@���@��F@��@���@�|�@�l�@�33@�"�@���@�^5@�@���@�@��^@��T@��@�-@��#@�z�@��
@��R@��!@�ȴ@��H@��@��R@�J@��@��T@���@��@���@�X@���@��@�~�@��-@���@�(�@�K�@�@��\@�~�@��@�C�@�ȴ@���@��@��@���@�hs@��@���@��-@��^@���@��#@��-@���@�z�@�r�@�A�@�9X@��@���@�\)@�33@���@�A�@�b@�1@���@�v�@�n�@�M�@���@�&�@��@���@���@�%@��@�Ĝ@���@��/@�Z@�b@���@�  @�\)@���@��@���@��@���@�A�@�o@�{@��^@�@��^@�x�@�z�@�r�@���@��@�Z@�  @���@�b@�z�@��
@��@�I�@��@��^@�J@�V@�v�@��+@��\@���@���@��\@�=q@�{@��@���@�O�@�?}@�/@�X@�X@��@���@�  @���@�S�@�
=@�ff@���@��@�bN@�  @l�@
=@~ȴ@~�R@~��@~v�@~V@}�@}�@}��@}�@|��@|�@{�
@{t�@|z�@|�@|9X@{�@{"�@{o@z=q@yx�@yhs@z{�@iN<1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A�  A�A�A�%A�A��;Aۛ�A�XA��A��A�M�Aӥ�A��;A��A�bNA���Aɥ�A���A�  A�~�A�1'A��DA�5?A���A��A�5?A�Q�A��A�oA���A���A��/A���A�"�A��#A�{A��TA�VA�hsA���A���A��`A���A�1'A���A�t�A�S�A�ffA��
A��A��A�r�A�(�A���A�;dA��wA���A�VA�r�A�oA���A��#A�$�A�bA�?}A���A�I�A���A��A��A���A�(�A�O�A��TA�t�A�~�A�A�5?A�VA��A~�\A}33Az��Ay�PAr�ApI�Aol�An1Al�Ai�-Ah�Ag�AehsAa�A^9XA\��A\VA[;dAY�AXȴAU�ATr�AS�FAS��AS7LAR5?AQC�AP��AO��AL��AK�FAJ�AI�7AG�7ADjAA��A?�
A>bA<Q�A:-A5�A4�RA3oA1��A/�mA/A-`BA+oA*�DA*E�A)��A'��A'�A$��A#�A!�A�A�`A��AI�A�TA�RA��AA|�A�`A=qA�TAA�A��AG�A�A�A�9AĜA
1'AȴA-Ap�AȴA �A��A�!A�A�A=qA �@�@�ff@��T@�X@�9X@�"�@�Z@�@�x�@�@�p�@�/@�@�o@�M�@�@�I�@ꟾ@��T@�ƨ@�+@���@� �@��
@��@�@�A�@��@��T@ݑh@�&�@܃@ۥ�@ڗ�@���@ם�@�&�@�K�@ҟ�@Ѳ-@�x�@�Q�@��@�-@̴9@�\)@�@ȋD@�l�@��y@Ƈ+@�E�@���@őh@�/@�/@��@�(�@+@��@�Q�@��m@��@�l�@�K�@�;d@��@��u@�@��\@�5?@���@�1'@��H@�  @�/@���@�S�@�o@���@��\@��@��m@�\)@�+@�"�@��@�@��!@�n�@�ff@�5?@��h@�/@��@��D@�1@���@���@�5?@�-@��@�O�@��@���@��@�z�@� �@���@�|�@�|�@��F@�dZ@�t�@��@��F@��@���@���@���@��F@��@���@�|�@�l�@�33@�"�@���@�^5@�@���@�@��^@��T@��@�-@��#@�z�@��
@��R@��!@�ȴ@��H@��@��R@�J@��@��T@���@��@���@�X@���@��@�~�@��-@���@�(�@�K�@�@��\@�~�@��@�C�@�ȴ@���@��@��@���@�hs@��@���@��-@��^@���@��#@��-@���@�z�@�r�@�A�@�9X@��@���@�\)@�33@���@�A�@�b@�1@���@�v�@�n�@�M�@���@�&�@��@���@���@�%@��@�Ĝ@���@��/@�Z@�b@���@�  @�\)@���@��@���@��@���@�A�@�o@�{@��^@�@��^@�x�@�z�@�r�@���@��@�Z@�  @���@�b@�z�@��
@��@�I�@��@��^@�J@�V@�v�@��+@��\@���@���@��\@�=q@�{@��@���@�O�@�?}@�/@�X@�X@��@���@�  @���@�S�@�
=@�ff@���@��@�bN@�  @l�@
=@~ȴ@~�R@~��@~v�@~V@}�@}�@}��@}�@|��@|�@{�
@{t�@|z�@|�@|9X@{�@{"�@{o@z=q@yx�@yhs@z{�@iN<1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BBB�B5?BI�BW
BZBt�B� B�B�%B�BjBbNBl�Bx�B� B� B~�B�1B�+B�1B�VB�VB�VB�JB�DB�7B�%B�B�By�Bt�Bn�BcTBXBR�BN�BH�B6FB%�B�BB�)B��B��BȴBĜB�qB�LB�!B��B�oB�B~�Bw�BjBT�BC�B8RB-B�B+B
��B
�B
�)B
��B
ÖB
�jB
�9B
��B
�B
t�B
^5B
B�B
5?B
+B
�B
JB	�sB	��B	��B	ĜB	�LB	��B	��B	��B	�=B	w�B	e`B	]/B	YB	R�B	K�B	D�B	7LB	/B	)�B	(�B	&�B	!�B	�B	�B	uB	1B	  B��B��B�B�ZB�)B��B��BŢB�wB�9B�B�B��B��B��B��B��B�B��B��B��B��B��B�bB�DB�7B�VB�hB��B��B��B��B�bB�7B�B�B�B�Bx�Bp�Bo�Bm�Bl�Bn�BgmBW
BO�BK�BI�BG�BE�BD�BB�B?}B=qB:^B8RB7LB6FB5?B49B33B2-B1'B0!B0!B/B/B/B/B-B,B,B,B+B)�B,B,B,B,B+B+B+B+B+B-B-B-B-B.B-B-B,B-B/B0!B2-B1'B33B33B33B33B5?B6FB7LB7LB8RB9XB:^B<jB?}BB�BD�BH�BN�BQ�B[#BbNBe`BiyBm�Bp�Bp�Bp�BhsBe`BgmBk�Bp�Bs�Br�Bp�Bt�Bx�By�Bz�B{�B|�B�B�+B�=B�DB�DB�DB�DB�PB�bB�bB�hB�oB�oB�uB�{B��B��B��B��B��B�B�!B�3B�FB�LB�LB�qBƨBɺB��B�B�;B�`B�B�B�B�B�B��B��B��B	B	%B	+B	PB	bB	�B	�B	 �B	"�B	#�B	&�B	(�B	)�B	,B	-B	/B	1'B	33B	49B	6FB	:^B	>wB	D�B	J�B	L�B	M�B	N�B	Q�B	S�B	W
B	]/B	`BB	dZB	gmB	ffB	dZB	cTB	dZB	e`B	gmB	m�B	r�B	r�B	p�B	p�B	r�B	w�B	w�B	|�B	�B	�B	�B	�%B	�7B	�DB	�DB	�DB	�JB	�VB	�bB	�hB	�hB	�hB	H�B	�B	�B	�B	�B	�qB	��B	��B	ÖB	ŢB	ÖB	ĜB	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ƨB	��B	�wB	�}B	ŢB	��B	ȴB	ƨB	ŢB	ŢB	ŢB	ŢB	ÖB	ÖB	ÖB	ĜB	ɺB	��B	��B	��B	��B	��B	��B	�
B	�)B	�HB	�TB	�fB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
SB
M2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222442222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BBB�B5?BI�BW
BZBt�B� B�B�%B�BjBbNBl�Bx�B� B� B~�B�1B�+B�1B�VB�VB�VB�JB�DB�7B�%B�B�By�Bt�Bn�BcTBXBR�BN�BH�B6FB%�B�BB�)B��B��BȴBĜB�qB�LB�!B��B�oB�B~�Bw�BjBT�BC�B8RB-B�B+B
��B
�B
�)B
��B
ÖB
�jB
�9B
��B
�B
t�B
^5B
B�B
5?B
+B
�B
JB	�sB	��B	��B	ĜB	�LB	��B	��B	��B	�=B	w�B	e`B	]/B	YB	R�B	K�B	D�B	7LB	/B	)�B	(�B	&�B	!�B	�B	�B	uB	1B	  B��B��B�B�ZB�)B��B��BŢB�wB�9B�B�B��B��B��B��B��B�B��B��B��B��B��B�bB�DB�7B�VB�hB��B��B��B��B�bB�7B�B�B�B�Bx�Bp�Bo�Bm�Bl�Bn�BgmBW
BO�BK�BI�BG�BE�BD�BB�B?}B=qB:^B8RB7LB6FB5?B49B33B2-B1'B0!B0!B/B/B/B/B-B,B,B,B+B)�B,B,B,B,B+B+B+B+B+B-B-B-B-B.B-B-B,B-B/B0!B2-B1'B33B33B33B33B5?B6FB7LB7LB8RB9XB:^B<jB?}BB�BD�BH�BN�BQ�B[#BbNBe`BiyBm�Bp�Bp�Bp�BhsBe`BgmBk�Bp�Bs�Br�Bp�Bt�Bx�By�Bz�B{�B|�B�B�+B�=B�DB�DB�DB�DB�PB�bB�bB�hB�oB�oB�uB�{B��B��B��B��B��B�B�!B�3B�FB�LB�LB�qBƨBɺB��B�B�;B�`B�B�B�B�B�B��B��B��B	B	%B	+B	PB	bB	�B	�B	 �B	"�B	#�B	&�B	(�B	)�B	,B	-B	/B	1'B	33B	49B	6FB	:^B	>wB	D�B	J�B	L�B	M�B	N�B	Q�B	S�B	W
B	]/B	`BB	dZB	gmB	ffB	dZB	cTB	dZB	e`B	gmB	m�B	r�B	r�B	p�B	p�B	r�B	w�B	w�B	|�B	�B	�B	�B	�%B	�7B	�DB	�DB	�DB	�JB	�VB	�bB	�hB	�hB	�hB	H�B	�B	�B	�B	�B	�qB	��B	��B	ÖB	ŢB	ÖB	ĜB	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ƨB	��B	�wB	�}B	ŢB	��B	ȴB	ƨB	ŢB	ŢB	ŢB	ŢB	ÖB	ÖB	ÖB	ĜB	ɺB	��B	��B	��B	��B	��B	��B	�
B	�)B	�HB	�TB	�fB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
SB
M2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222442222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.33 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191719                              AO  ARCAADJP                                                                    20181005191719    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191719  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191719  QCF$                G�O�G�O�G�O�C000            