CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   R   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-04-06T02:00:49Z creation      
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
resolution        >�EȠ     
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ     
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
resolution        =���   axis      Z        H  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  T  :�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  ;   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  T  <T   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  <�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  =�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  T  ?8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  ?�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  T  @�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  A(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  Bp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  T  C�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  T  ET   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  E�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  F�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    G    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    J    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    M    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  P    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    PL   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    PP   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    PT   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    PX   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  P\   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    P�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    P�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    P�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         P�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         P�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        P�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    P�Argo profile    3.1 1.2 19500101000000  20160413055314  20190726102812  3900734 US ARGO PROJECT                                                 BRECK OWENS                                                     PRES            TEMP            PSAL               �A   AO  2766_71621_189                  2C  D   SOLO_W                          SL813                           1.10                            851 @ֈ�Y L�1   @ֈ��"�u�DY7KƧ�@D�z�G�1   ARGOS   Primary sampling: averaged [data averaged with equal weights into irregular pressure bins                                                                                                                                                                          A   A   A   @�  A   Ap  A�  A�  A�  B  B   B4  BH  B\  Bp  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C  C  C  C  C  C  C   C%  C*  C/  C4  C9  C>  CC  CH  C\  Cp  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D  D� D  D'� D4  D@� DM  DY� Df  Dr� D  D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ Dʀ D�� D�  D�@ D� D�� D�  D�@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  A   Ap  A�  A�  A�  B  B   B4  BH  B\  Bp  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C  C  C  C  C  C  C   C%  C*  C/  C4  C9  C>  CC  CH  C\  Cp  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D  D� D  D'� D4  D@� DM  DY� Df  Dr� D  D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ Dʀ D�� D�  D�@ D� D�� D�  D�@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Ar��Ar1Aq|�Ap�!Aop�Ao|�Al��Ai�Ah�9Ahv�Ag�Aa�A]�AS�PAM�AJ�yAJjAHM�AIoAHQ�AH1AF9XAC�ABZA@��A=�
A;�7A7�A7
=A3�7A0�A/
=A(��A$��A#��A$  A$z�A$~�A#�A"�A�-A1'A�A�wA	��A`BAA�AI�A\)A ^5@�|�@���@�M�@�x�@���@�~�@���@���@�;d@��R@�  @i��@]?}@X�9@S��@M�h@J~�@D�j@@��@D�/@>�+@=/@:��@7�;@5�h@3C�@1x�@/��@.E�@,9X@)��@(b1111111111111111111111111111111111111111111111111111111111111111111111111111111111  Ar��Ar1Aq|�Ap�!Aop�Ao|�Al��Ai�Ah�9Ahv�Ag�Aa�A]�AS�PAM�AJ�yAJjAHM�AIoAHQ�AH1AF9XAC�ABZA@��A=�
A;�7A7�A7
=A3�7A0�A/
=A(��A$��A#��A$  A$z�A$~�A#�A"�A�-A1'A�A�wA	��A`BAA�AI�A\)A ^5@�|�@���@�M�@�x�@���@�~�@���@���@�;d@��R@�  @i��@]?}@X�9@S��@M�h@J~�@D�j@@��@D�/@>�+@=/@:��@7�;@5�h@3C�@1x�@/��@.E�@,9X@)��@(b1111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�/B	�/B	�B	�B
B
2-B
:^B
7LB
]/B
k�B
p�B
q�B
�!B
��B
�!B  B.Bq�B�jBhB:^BK�BG�B?}B2-B�B��B�B�B�^B��B�DBD�B�B�B�B%�B+B�B�B
�'B
bNB
49B
(�B
�B
oB
�B
B
�B
�B	��B	�XB	�PB	H�B	6FB	H�B	e`B	��B	��B	��B	��B	}�B	�B	��B	��B	�BB	��B
�B
49B
\)B
p�B
�B
�oB
��B
�!B
��B
��B
�B
�`B
�B
��B11111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�B	��B	�XB	�B
#B
4�B
>WB
8B
]^B
l�B
vB
w B
�TB
��B
��B B0vBp�B�AB�B<ABN�BIBA=B5}B@B�B�yB��B�kB�B�xBIVB �B�B0B%�B,�BmB�B
�gB
c�B
4�B
)�B
 xB
�B
�B
rB
�B
�B	�"B	��B	��B	I�B	6�B	I/B	e�B	��B	�3B	�B	��B	~qB	�FB	��B	��B	�cB	�)B
�B
4"B
\ZB
p�B
�3B
��B
��B
�8B
��B
��B
�,B
�qB
��B
�
B41111111111111111111111111111111111111111111111111111111111111111111111111111111111  <$ub<$(<$f�<%e<#��<)'7</��<$B�<#��<%v�<9��<9��<g>k<?�<-�M<$<<(7�<$E<$c�<#�<&�
<+�<%K:<&7"<,2�<);-<6��<$ �</[0<*��<'�T<HP�<4s�<$�X<#�X<$r<#�i<%��<$:�<(_�<'��<%��<$�<$k<$@|<#��<$�<#�5<#�<%S�<%\\<$�<%(<$�j<$�<$/<#��<#�N<$�<$<<$�V<$�<#�J<#�^<#�<#�]<#ߜ<#ۮ<#ا<#�^<#��<#�C<#ٛ<#�c<#ا<#�$<#�<#׺<#��<#ا<#��<#�PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Conductivity Thermal Mass (CTM), Johnson et al., 2007, JAOT                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s, rise rate = 10 cm/s with error equal to the adjustment                                                                                                                                                                            No significant pressure drift detected                                                                                                                                                                                                                          No significant temperature drift detected                                                                                                                                                                                                                       No significant conductivity drift detected                                                                                                                                                                                                                      201801291535072018012915350720180129153507  AO  ARGQQCPL                                                                    20160413055314  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160413055314  QCF$                G�O�G�O�G�O�0               WHOIARSQWHQCV0.3                                                                20180125073759  QC                  G�O�G�O�G�O�                WHOIARSQ CTMV1.0                                                                20180126105059  IP                  G�O�G�O�G�O�                WHOIARSQCLIMN/A CSIRO,ArgoCARS2016                                              20180129000000  IP                  G�O�G�O�G�O�                WHOIARDUV_31V1.0                                                                20190726102812  UP                  G�O�G�O�G�O�                