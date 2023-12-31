CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:40Z creation      
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
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181024140840  20181024140840  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @�ޤ˩��1   @�ޥhK��@5o�;dZ�c���R1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�  @�  A   A   A@  Aa��A���A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B���B�ffB���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�33B�33B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C'�fC*  C+�fC.  C0  C2  C3�fC5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  D   D y�D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%�fD&  D&� D'  D'�fD(  D(� D)  D)y�D*  D*�fD+fD+� D,  D,� D-  D-� D.  D.�fD/  D/y�D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6y�D7  D7� D8  D8� D9fD9� D:  D:� D:��D;y�D<  D<� D=  D=� D>  D>� D?fD?� D@  D@� DA  DAy�DB  DBy�DB��DC� DD  DD� DE  DE�fDFfDF�fDG  DG� DH  DH� DI  DIy�DJ  DJ� DK  DK� DK��DLy�DL��DM� DN  DN� DO  DO� DP  DP�fDQfDQ�fDR  DR� DS  DS�fDT  DT� DU  DU� DV  DV� DW  DW� DX  DXy�DY  DY� DZ  DZ� D[  D[y�D\  D\� D]  D]y�D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� De��Df� Dg  Dg� Dg��Dh� Di  Di� DjfDj�fDk  Dk� Dl  Dl�fDm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� DwٚDy�
D�(�D��
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�Q�@�Q�A(�A$(�AD(�AeA��HA��HA�{A�{A�{A�{A�{A�{B
=B	
=B
=B
=B!
=B)
=B1
=B9
=BAp�BI
=BQ
=BY
=Ba
=Bi
=Bq
=By
=B��B��B��B��B��RB��B��B�Q�B�Q�B��B��B��B��B��B��B��B��BąBȅB̅BЅBԅB�Q�B܅B�RB�RB�Q�B�B��B�B��B��C B�CB�CB�CB�CB�C
B�CB�CB�CB�CB�CB�CB�CB�CB�CB�CB�C B�C"B�C$B�C&B�C((�C*B�C,(�C.B�C0B�C2B�C4(�C6(�C8B�C:B�C<B�C>B�C@B�CBB�CDB�CFB�CHB�CJB�CLB�CNB�CPB�CRB�CTB�CVB�CXB�CZB�C\B�C^B�C`B�CbB�CdB�CfB�ChB�CjB�ClB�CnB�CpB�CrB�CtB�CvB�CxB�CzB�C|B�C~B�C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�.C�!HC�{C�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�.C�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�{C�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�{C�!HC�!HC�!HD �D �>D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D
D��D�D��D�D��D�D��D�D��D
>D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%�
D&�D&��D'�D'�
D(�D(��D)�D)�>D*�D*�
D+
D+��D,�D,��D-�D-��D.�D.�
D/�D/�>D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6�>D7�D7��D8�D8��D9
D9��D:�D:��D;
>D;�>D<�D<��D=�D=��D>�D>��D?
D?��D@�D@��DA�DA�>DB�DB�>DC
>DC��DD�DD��DE�DE�
DF
DF�
DG�DG��DH�DH��DI�DI�>DJ�DJ��DK�DK��DL
>DL�>DM
>DM��DN�DN��DO�DO��DP�DP�
DQ
DQ�
DR�DR��DS�DS�
DT�DT��DU�DU��DV�DV��DW�DW��DX�DX�>DY�DY��DZ�DZ��D[�D[�>D\�D\��D]�D]�>D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df
>Df��Dg�Dg��Dh
>Dh��Di�Di��Dj
Dj�
Dk�Dk��Dl�Dl�
Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq�>Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dw�>Dy��D�0�D��\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��TA��;A��;A���A���A��/A��;A��TA��HA��/A��
A��/A��/A��;A��`A��yA��yA��yA��A��A��A��A��A��A��A��yA��`A��A�ȴAθRA�jA�E�A�v�A��HA�
=A�{A���A���A�VA���A���A���A���A�n�A�ZA�-A���A��A���A���A�p�A�ƨA�bNA���A���A�jA���A�=qA�ZA�
=A��PA�/A���A��A��A��#A���A�  A�  A��A���A�M�A�bA�VA�Q�A�
=A�C�A��hA���A���A��uA� �A���A�;dA��A�Q�A�"�A�oA�A�&�A���A�ƨA���A� �A��A�^5A�x�A�hsA���A���A��A���A�oA~�A~A�A}�PA|�A{��Azr�Ay
=Aw?}At�Ar�Aq�mAqAn�AjA�Ag+Aa��A_�A]��A[��AZ �AY�FAY&�AXȴAXJAW��AUO�AS"�AQ�
AP��APz�AO�AOp�AN��AM��AL^5AKO�AHI�AE7LAC|�ABQ�AAdZA>A�A;�hA9\)A85?A7&�A5�A5%A3O�A1�;A1VA0  A.�A-%A,ZA+��A)��A(  A&�!A&5?A%��A%`BA#�A#��A"��A"A�A!VA �A33A(�A��A��A�hAt�A7LA�AQ�A�\A�`A��A
=A�
Ax�AdZA+A��A~�A�A�A�FA`BAȴA��An�AJA��A��Ar�AA�A�^AdZA
��A
v�A	|�AQ�A�7AA�A��AXA�A"�A�A {@���@���@���@��w@�E�@�r�@�@�@�7L@�@�=q@�@�V@�@���@�$�@�K�@�O�@��@���@��@���@���@�$�@��/@Դ9@ԛ�@ԛ�@�(�@��@�n�@Ѳ-@�O�@�V@Ь@�1'@��@ύP@�K�@Ο�@���@��@�(�@��R@���@��7@���@�X@���@�|�@��@���@�l�@�33@�@��@���@���@�v�@�M�@��j@��@��@��@�@��-@�hs@�Ĝ@�j@�I�@�  @���@�dZ@�o@�$�@��-@�7L@�r�@� �@��@��@��
@���@���@�ƨ@��@���@�bN@�{@��^@��@��@�33@�dZ@�j@��9@�Ĝ@���@��`@���@���@�V@�&�@�`B@��h@��^@��h@�hs@�X@�`B@�hs@�p�@��@��@��@�x�@�x�@�p�@�x�@�x�@��@�p�@��@���@��@���@�-@���@�/@�j@���@�o@��@�l�@�ƨ@��w@�
=@��@���@��j@���@�r�@��F@���@�=q@���@�hs@�&�@�%@��@�j@�1'@�b@��@�C�@�
=@��@�n�@��@��@�x�@�?}@�?}@�/@�&�@���@��`@��/@��/@��/@��u@�bN@��@��@�K�@��@�~�@��#@��^@��T@�J@���@��#@��-@��7@�/@�V@���@��@��`@��`@���@���@��D@�A�@���@�t�@�K�@�S�@�K�@�o@�@�@��@��H@���@��R@���@���@�~�@�^5@�M�@�E�@�J@���@�&�@��/@�r�@��@��w@��!@��@�@��7@��@��9@�z�@�bN@� �@��F@���@�$�@���@��^@�x�@�/@��/@��j@�Q�@�\)@��y@���@�M�@��T@�hs@�&�@�V@�V@���@���@���@�bN@�1@�b@��P@�K�@�c�@w�;@g1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��TA��;A��;A���A���A��/A��;A��TA��HA��/A��
A��/A��/A��;A��`A��yA��yA��yA��A��A��A��A��A��A��A��yA��`A��A�ȴAθRA�jA�E�A�v�A��HA�
=A�{A���A���A�VA���A���A���A���A�n�A�ZA�-A���A��A���A���A�p�A�ƨA�bNA���A���A�jA���A�=qA�ZA�
=A��PA�/A���A��A��A��#A���A�  A�  A��A���A�M�A�bA�VA�Q�A�
=A�C�A��hA���A���A��uA� �A���A�;dA��A�Q�A�"�A�oA�A�&�A���A�ƨA���A� �A��A�^5A�x�A�hsA���A���A��A���A�oA~�A~A�A}�PA|�A{��Azr�Ay
=Aw?}At�Ar�Aq�mAqAn�AjA�Ag+Aa��A_�A]��A[��AZ �AY�FAY&�AXȴAXJAW��AUO�AS"�AQ�
AP��APz�AO�AOp�AN��AM��AL^5AKO�AHI�AE7LAC|�ABQ�AAdZA>A�A;�hA9\)A85?A7&�A5�A5%A3O�A1�;A1VA0  A.�A-%A,ZA+��A)��A(  A&�!A&5?A%��A%`BA#�A#��A"��A"A�A!VA �A33A(�A��A��A�hAt�A7LA�AQ�A�\A�`A��A
=A�
Ax�AdZA+A��A~�A�A�A�FA`BAȴA��An�AJA��A��Ar�AA�A�^AdZA
��A
v�A	|�AQ�A�7AA�A��AXA�A"�A�A {@���@���@���@��w@�E�@�r�@�@�@�7L@�@�=q@�@�V@�@���@�$�@�K�@�O�@��@���@��@���@���@�$�@��/@Դ9@ԛ�@ԛ�@�(�@��@�n�@Ѳ-@�O�@�V@Ь@�1'@��@ύP@�K�@Ο�@���@��@�(�@��R@���@��7@���@�X@���@�|�@��@���@�l�@�33@�@��@���@���@�v�@�M�@��j@��@��@��@�@��-@�hs@�Ĝ@�j@�I�@�  @���@�dZ@�o@�$�@��-@�7L@�r�@� �@��@��@��
@���@���@�ƨ@��@���@�bN@�{@��^@��@��@�33@�dZ@�j@��9@�Ĝ@���@��`@���@���@�V@�&�@�`B@��h@��^@��h@�hs@�X@�`B@�hs@�p�@��@��@��@�x�@�x�@�p�@�x�@�x�@��@�p�@��@���@��@���@�-@���@�/@�j@���@�o@��@�l�@�ƨ@��w@�
=@��@���@��j@���@�r�@��F@���@�=q@���@�hs@�&�@�%@��@�j@�1'@�b@��@�C�@�
=@��@�n�@��@��@�x�@�?}@�?}@�/@�&�@���@��`@��/@��/@��/@��u@�bN@��@��@�K�@��@�~�@��#@��^@��T@�J@���@��#@��-@��7@�/@�V@���@��@��`@��`@���@���@��D@�A�@���@�t�@�K�@�S�@�K�@�o@�@�@��@��H@���@��R@���@���@�~�@�^5@�M�@�E�@�J@���@�&�@��/@�r�@��@��w@��!@��@�@��7@��@��9@�z�@�bN@� �@��F@���@�$�@���@��^@�x�@�/@��/@��j@�Q�@�\)@��y@���@�M�@��T@�hs@�&�@�V@�V@���@���@���@�bN@�1@�b@��P@�K�@�c�@w�;@g1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B5?B6FB5?B6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB7LB7LB7LB8RB8RB:^B;dB:^B;dB@�BD�BJ�BYBjBz�B�XB��B	7BVB�B�B$�B@�BH�BVBYBYBXBXBW
BVBVB]/BcTBgmBo�Bq�Bs�Bt�Bu�Bt�Bq�BjBdZBYBK�B=qB8RB2-B&�B{B
=BB��B��B�B��B�-B��B��B�%Bx�B`BBT�BE�B5?B.B)�B�BPB%BBBB
�B
�/B
��B
�wB
�B
��B
�{B
�7B
w�B
[#B
L�B
B�B
.B
&�B
�B
�B
�B
hB
DB
B	��B	�B	�ZB	�B	��B	��B	�dB	��B	�bB	s�B	ffB	_;B	T�B	L�B	J�B	G�B	D�B	A�B	<jB	33B	)�B	#�B	 �B	�B	�B	�B	{B	bB	
=B	B��B�B�fB�HB�/B��B��BɺBŢBB��B�wB�qB�^B�LB�9B�!B�B�B��B��B��B��B��B��B��B��B��B�{B�bB�7B�B~�B}�B}�B}�B}�B}�B|�B|�B|�B�B�B�%B�B|�B~�B� B�B�+B�1B�DB�DB�JB�JB�JB�JB�=B�+B�B� B}�B|�Bz�Bx�Bw�Bt�Bp�Bl�BjBm�Bn�Bo�Bo�Bk�BiyBhsBhsBiyBjBl�Bo�Bt�By�By�Bx�Bv�Bv�Bu�Bt�Bp�BjBcTB_;BaHBbNB^5B\)B_;BaHBbNBcTBcTBcTBcTBdZBffBgmBhsBiyBiyBjBk�Bl�Bm�Bp�Bo�B�B�%B�uB��B��B��B��B��B�B�B��B��B��B�B�B�'B�-B�9B�?B�FB��BƨBƨBǮB��B��B��B��B�
B�B�;B�NB�TB�fB�B�B�B�B��B��B��B��B��B��B	  B	B	B	oB	.B	7LB	>wB	G�B	I�B	K�B	W
B	]/B	bNB	dZB	e`B	ffB	ffB	gmB	iyB	n�B	t�B	x�B	|�B	� B	�B	�B	�B	�B	�%B	�+B	�1B	�1B	�7B	�=B	�JB	�PB	�\B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�{B	��B	��B	��B	��B	��B	�{B	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�3B	�3B	�9B	�?B	�?B	�?B	�?B	�?B	�LB	�RB	�dB	�qB	�wB	��B	ĜB	ǮB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�/B	�;B	�BB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�mB	�mB	�mB	�sB	�sB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
	B
!�B
,�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B5?B6FB5?B6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB7LB7LB7LB8RB8RB:^B;dB:^B;dB@�BD�BJ�BYBjBz�B�XB��B	7BVB�B�B$�B@�BH�BVBYBYBXBXBW
BVBVB]/BcTBgmBo�Bq�Bs�Bt�Bu�Bt�Bq�BjBdZBYBK�B=qB8RB2-B&�B{B
=BB��B��B�B��B�-B��B��B�%Bx�B`BBT�BE�B5?B.B)�B�BPB%BBBB
�B
�/B
��B
�wB
�B
��B
�{B
�7B
w�B
[#B
L�B
B�B
.B
&�B
�B
�B
�B
hB
DB
B	��B	�B	�ZB	�B	��B	��B	�dB	��B	�bB	s�B	ffB	_;B	T�B	L�B	J�B	G�B	D�B	A�B	<jB	33B	)�B	#�B	 �B	�B	�B	�B	{B	bB	
=B	B��B�B�fB�HB�/B��B��BɺBŢBB��B�wB�qB�^B�LB�9B�!B�B�B��B��B��B��B��B��B��B��B��B�{B�bB�7B�B~�B}�B}�B}�B}�B}�B|�B|�B|�B�B�B�%B�B|�B~�B� B�B�+B�1B�DB�DB�JB�JB�JB�JB�=B�+B�B� B}�B|�Bz�Bx�Bw�Bt�Bp�Bl�BjBm�Bn�Bo�Bo�Bk�BiyBhsBhsBiyBjBl�Bo�Bt�By�By�Bx�Bv�Bv�Bu�Bt�Bp�BjBcTB_;BaHBbNB^5B\)B_;BaHBbNBcTBcTBcTBcTBdZBffBgmBhsBiyBiyBjBk�Bl�Bm�Bp�Bo�B�B�%B�uB��B��B��B��B��B�B�B��B��B��B�B�B�'B�-B�9B�?B�FB��BƨBƨBǮB��B��B��B��B�
B�B�;B�NB�TB�fB�B�B�B�B��B��B��B��B��B��B	  B	B	B	oB	.B	7LB	>wB	G�B	I�B	K�B	W
B	]/B	bNB	dZB	e`B	ffB	ffB	gmB	iyB	n�B	t�B	x�B	|�B	� B	�B	�B	�B	�B	�%B	�+B	�1B	�1B	�7B	�=B	�JB	�PB	�\B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�{B	��B	��B	��B	��B	��B	�{B	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�3B	�3B	�9B	�?B	�?B	�?B	�?B	�?B	�LB	�RB	�dB	�qB	�wB	��B	ĜB	ǮB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�/B	�;B	�BB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�mB	�mB	�mB	�sB	�sB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
	B
!�B
,�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140840                              AO  ARCAADJP                                                                    20181024140840    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140840  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140840  QCF$                G�O�G�O�G�O�0               