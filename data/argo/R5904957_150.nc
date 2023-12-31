CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:32Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140832  20181024140832  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��$���U1   @��%l� @5R-V�c���R1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @���@�33A   AffA@  Aa��A�  A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BHffBO��BW��B`ffBh  Bp  Bw��B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  C   C  C  C  C  C
�C�C  C  C  C  C  C  C�fC�fC  C   C"  C$  C&  C(  C*  C+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cq�fCt  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  D   D �fD  D� D  D� D  D� D  Dy�D  D� D  D�fD  D� D  D� D	  D	� D	��D
y�D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D  D� D  D�fD  D� D  D� D  D�fD  D� D  D� D��D� D   D � D!  D!� D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+fD+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7y�D7��D8� D9fD9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DA��DB� DC  DC� DD  DD� DD��DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DK��DL� DM  DM� DN  DN� DN��DOy�DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT�fDUfDU� DV  DV� DW  DW� DX  DX� DY  DY� DZfDZ�fD[fD[�fD\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Da��Db� Dc  Dc� Dd  Dd� Dd��De� Df  Df� DgfDg�fDh  Dh� Di  Di� Dj  Dj� Dk  Dk�fDl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� DwfDw� Dw��Dy� D�ED�o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�p�@�
=A�A Q�AA�Ac�A���A���A�(�A���A���A���A���A���B z�Bz�Bz�Bz�B z�B(z�B0z�B8�GB@z�BH�GBP{BX{B`�GBhz�Bpz�Bx{B�=qB�=qB�p�B�p�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�
>B�=qB�=qB�=qB�=qB�=qB�=qB�
>B�
>B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�
>B�=qB�=qB�=qB�=qC �C�C�C�C�C
8RC8RC�C�C�C�C�C�CCC�C �C"�C$�C&�C(�C*�C,C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�CrCt�Cv�Cx�Cz�C|�C~�C�\C�\C�\C��C�\C�\C��C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C��C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C��C��C�\C�)C�)C�\C�\C�\C��C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�)C�\C�\C�\C�\C��C�\C�\C��C�\C�)C�)C�)C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C��C��C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�)C�)C�\C�\C�\C�\C�\C�\C�\C��C�\C�)C�)C�\D �D �D�D��D�D��D�D��D�D�HD�D��D�D�D�D��D�D��D	�D	��D
HD
�HD�D��D�D��D�D��D�D��D�D��DD��D�D��D�D��D�D��D�D��D�D��DHD�HD�D��D�D��D�D�D�D��D�D��D�D�D�D��D�D��DHD��D �D ��D!�D!��D"�D"��D#�D#��D$�D$�D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7�HD8HD8��D9D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DBHDB��DC�DC��DD�DD��DEHDE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DLHDL��DM�DM��DN�DN��DOHDO�HDP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT�DUDU��DV�DV��DW�DW��DX�DX��DY�DY��DZDZ�D[D[�D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��DbHDb��Dc�Dc��Dd�Dd��DeHDe��Df�Df��DgDg�Dh�Dh��Di�Di��Dj�Dj��Dk�Dk�Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq�HDr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��DwDw��Dw�{Dy��D�H�D�s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�C�A�G�A�E�A�C�A�S�A�XA�XA�ZA�XA�XA�XA�XA�XA�ZA�ZA�ZA�ZA�\)A�\)A�S�A�M�A�M�A�-A�`BA�p�Aş�A�;dA�A�\)A���A�I�A��A��A�O�A��A�&�A�x�A�VA�  A��yA�x�A���A�^5A�x�A��`A��A�JA���A�  A���A�n�A���A��PA�?}A���A���A�{A�?}A�G�A��A�M�A���A�+A��hA��FA���A��mA���A�^5A�
=A��PA��A�ZA���A��wA�5?A�$�A�E�A�x�A���A�\)A�(�A
=A|��AzbAx��AxE�Aw�-AwS�AvĜAu��At��As�FAs
=Ar�Aq��Ap  Aj9XAg��Ac�TAa��A`��A]�AY`BAV�yAT�DAQl�AP��ANv�AM��AL��AJ�AH��AG�AF��AE+AC�FAA�^AA7LA@ȴA@�A?p�A>��A=�7A<JA9��A8(�A6I�A4�A2�A.�A-t�A-S�A-�A,��A,�9A,��A,VA+`BA)��A(��A(M�A&�jA$�A#|�A"ffA!�A!K�A $�A�TA�-A�-A��A��A�7AdZAXAG�A�!A�A�/A�AȴAI�A(�A��A��A\)AC�A&�AI�A"�A^5A�^A|�A�
AXA;dA�A��A�uAJA��A�A%A^5A  A�wA�7A/A��AffA�
AhsA
��A
A�-A{A\)A�AĜAhsA�At�A
=A 1'@�33@��u@�K�@��T@�Ĝ@�\)@�-@�A�@�v�@�7L@�K�@��#@��@���@�E�@�@��H@�?}@�z�@��;@�@���@��@�j@�(�@�|�@��H@�`B@�1'@���@؃@� �@ם�@�~�@ա�@�`B@�O�@�/@���@�j@��H@��#@ЋD@��
@�@�=q@͉7@���@̼j@̓u@�r�@�A�@�1@�S�@ʏ\@���@�b@�dZ@��@���@�5?@�@�@�X@��`@�I�@��H@���@�?}@��@���@��9@�z�@�A�@���@�+@�^5@�5?@�?}@��@���@�  @��@�1@��@��@�1@�1@�1@�  @���@���@�ȴ@���@��@�ƨ@�V@��;@��@��y@���@��@�S�@�A�@��`@��`@��j@���@��F@���@�S�@��y@���@��@���@��P@�K�@��y@�E�@��^@�7L@��@���@��@�|�@��@���@�n�@�$�@�@�X@��@��@�Ĝ@��@��@���@�\)@�"�@��y@���@�E�@�=q@�=q@��T@�?}@���@���@�9X@��m@��P@�+@���@���@�V@��`@�Ĝ@��9@���@�z�@�9X@�  @��F@�t�@�S�@�C�@�"�@���@�=q@�=q@�{@��@��@�X@��@��/@��9@�z�@�9X@��@���@�+@�o@���@�v�@�E�@�-@���@��h@�`B@�?}@��`@���@��@�r�@�9X@� �@� �@��@��m@���@�l�@�dZ@�S�@��y@��!@��\@�ff@�~�@�~�@��@�@��^@�&�@���@��@�b@��@��
@���@���@��@�  @�  @�  @�  @���@��
@��@��@��H@�@�
=@��\@�V@�=q@��^@��@�O�@�?}@�7L@�&�@��9@��@�9X@��@�1'@�  @��@�|�@�S�@�o@���@��\@�5?@��T@�@���@�O�@��@���@��@�Ĝ@��u@�bN@�A�@��@�  @���@�\)@��y@���@�^5@���@�@��h@�`B@�O�@�/@�V@���@��@���@�|�@�dZ@�S�@�+@��y@�ȴ@��@|!@jȴ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�C�A�G�A�E�A�C�A�S�A�XA�XA�ZA�XA�XA�XA�XA�XA�ZA�ZA�ZA�ZA�\)A�\)A�S�A�M�A�M�A�-A�`BA�p�Aş�A�;dA�A�\)A���A�I�A��A��A�O�A��A�&�A�x�A�VA�  A��yA�x�A���A�^5A�x�A��`A��A�JA���A�  A���A�n�A���A��PA�?}A���A���A�{A�?}A�G�A��A�M�A���A�+A��hA��FA���A��mA���A�^5A�
=A��PA��A�ZA���A��wA�5?A�$�A�E�A�x�A���A�\)A�(�A
=A|��AzbAx��AxE�Aw�-AwS�AvĜAu��At��As�FAs
=Ar�Aq��Ap  Aj9XAg��Ac�TAa��A`��A]�AY`BAV�yAT�DAQl�AP��ANv�AM��AL��AJ�AH��AG�AF��AE+AC�FAA�^AA7LA@ȴA@�A?p�A>��A=�7A<JA9��A8(�A6I�A4�A2�A.�A-t�A-S�A-�A,��A,�9A,��A,VA+`BA)��A(��A(M�A&�jA$�A#|�A"ffA!�A!K�A $�A�TA�-A�-A��A��A�7AdZAXAG�A�!A�A�/A�AȴAI�A(�A��A��A\)AC�A&�AI�A"�A^5A�^A|�A�
AXA;dA�A��A�uAJA��A�A%A^5A  A�wA�7A/A��AffA�
AhsA
��A
A�-A{A\)A�AĜAhsA�At�A
=A 1'@�33@��u@�K�@��T@�Ĝ@�\)@�-@�A�@�v�@�7L@�K�@��#@��@���@�E�@�@��H@�?}@�z�@��;@�@���@��@�j@�(�@�|�@��H@�`B@�1'@���@؃@� �@ם�@�~�@ա�@�`B@�O�@�/@���@�j@��H@��#@ЋD@��
@�@�=q@͉7@���@̼j@̓u@�r�@�A�@�1@�S�@ʏ\@���@�b@�dZ@��@���@�5?@�@�@�X@��`@�I�@��H@���@�?}@��@���@��9@�z�@�A�@���@�+@�^5@�5?@�?}@��@���@�  @��@�1@��@��@�1@�1@�1@�  @���@���@�ȴ@���@��@�ƨ@�V@��;@��@��y@���@��@�S�@�A�@��`@��`@��j@���@��F@���@�S�@��y@���@��@���@��P@�K�@��y@�E�@��^@�7L@��@���@��@�|�@��@���@�n�@�$�@�@�X@��@��@�Ĝ@��@��@���@�\)@�"�@��y@���@�E�@�=q@�=q@��T@�?}@���@���@�9X@��m@��P@�+@���@���@�V@��`@�Ĝ@��9@���@�z�@�9X@�  @��F@�t�@�S�@�C�@�"�@���@�=q@�=q@�{@��@��@�X@��@��/@��9@�z�@�9X@��@���@�+@�o@���@�v�@�E�@�-@���@��h@�`B@�?}@��`@���@��@�r�@�9X@� �@� �@��@��m@���@�l�@�dZ@�S�@��y@��!@��\@�ff@�~�@�~�@��@�@��^@�&�@���@��@�b@��@��
@���@���@��@�  @�  @�  @�  @���@��
@��@��@��H@�@�
=@��\@�V@�=q@��^@��@�O�@�?}@�7L@�&�@��9@��@�9X@��@�1'@�  @��@�|�@�S�@�o@���@��\@�5?@��T@�@���@�O�@��@���@��@�Ĝ@��u@�bN@�A�@��@�  @���@�\)@��y@���@�^5@���@�@��h@�`B@�O�@�/@�V@���@��@���@�|�@�dZ@�S�@�+@��y@�ȴ@��@|!@jȴ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�yB�yB�yB�yB�yB�yB�B�yB�B�B�B�B�B�B�B�B�B�B�yB�B��B�B"�B'�B/B/B49BB�BC�BA�B?}B>wB=qB>wB9XB33B)�B"�B�B
=BBB��B�B�fB��B�9B�B�B��B��B��B�JBv�BaHBS�BJ�BC�B=qB5?B+B�B�B{BbB  B
�fB
��B
ȴB
�RB
��B
��B
�VB
t�B
`BB
G�B
8RB
&�B
oB
%B	��B	�B	�B	�mB	�`B	�HB	�#B	�B	��B	��B	ȴB	B	�?B	��B	�7B	w�B	l�B	e`B	W
B	C�B	7LB	-B	�B	�B	oB	JB	+B��B�B�B�sB�B�B�B�yB�fB�ZB�NB�;B�#B�
B��BɺBÖB�qB�LB�'B�!B�!B�!B�!B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B��B��B��B��B�B�B�B�B�B�B�B�B�B�'B�3B�9B�3B�3B�3B�3B�-B�!B�B�B��B��B��B��B��B��B��B�\B�\B�bB�hB�\B�PB�DB�DB�JB�JB�PB�\B�bB�bB�oB�uB�uB�{B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B�B�B�B�-B�?B�FB�FB�FB�LB�RB�}B��BĜBƨBȴB��B��B��B��B��B��B��B��B�B�B�5B�HB�ZB�ZB�`B�mB�mB�sB�B�B��B��B��B��B��B	  B	B	B	B	B	B	B	B	  B��B	  B	B	B	
=B	PB	PB	\B	oB	oB	oB	uB	{B	�B	�B	�B	uB	VB	+B		7B	
=B	DB	JB	VB	�B	�B	#�B	%�B	+B	+B	,B	-B	/B	5?B	=qB	>wB	>wB	?}B	@�B	E�B	I�B	K�B	K�B	K�B	K�B	L�B	N�B	O�B	Q�B	S�B	VB	XB	YB	ZB	[#B	[#B	[#B	^5B	bNB	cTB	e`B	gmB	l�B	n�B	n�B	r�B	y�B	{�B	~�B	�B	�B	�B	�%B	�+B	�+B	�DB	�PB	�PB	�PB	�VB	�\B	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�9B	�?B	�LB	�RB	�RB	�jB	�}B	��B	��B	��B	B	B	ÖB	ÖB	ĜB	ŢB	ƨB	ƨB	ƨB	ƨB	ǮB	ǮB	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�/B	�5B	�5B	�;B	�HB	�ZB	�`B	�`B	�fB	�fB	�mB	�fB	�fB	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
DB
JB
JB
JB
JB
JB
JB
�B
�B
(�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�yB�yB�yB�yB�yB�yB�B�yB�B�B�B�B�B�B�B�B�B�B�yB�B��B�B"�B'�B/B/B49BB�BC�BA�B?}B>wB=qB>wB9XB33B)�B"�B�B
=BBB��B�B�fB��B�9B�B�B��B��B��B�JBv�BaHBS�BJ�BC�B=qB5?B+B�B�B{BbB  B
�fB
��B
ȴB
�RB
��B
��B
�VB
t�B
`BB
G�B
8RB
&�B
oB
%B	��B	�B	�B	�mB	�`B	�HB	�#B	�B	��B	��B	ȴB	B	�?B	��B	�7B	w�B	l�B	e`B	W
B	C�B	7LB	-B	�B	�B	oB	JB	+B��B�B�B�sB�B�B�B�yB�fB�ZB�NB�;B�#B�
B��BɺBÖB�qB�LB�'B�!B�!B�!B�!B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B��B��B��B��B�B�B�B�B�B�B�B�B�B�'B�3B�9B�3B�3B�3B�3B�-B�!B�B�B��B��B��B��B��B��B��B�\B�\B�bB�hB�\B�PB�DB�DB�JB�JB�PB�\B�bB�bB�oB�uB�uB�{B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B�B�B�B�-B�?B�FB�FB�FB�LB�RB�}B��BĜBƨBȴB��B��B��B��B��B��B��B��B�B�B�5B�HB�ZB�ZB�`B�mB�mB�sB�B�B��B��B��B��B��B	  B	B	B	B	B	B	B	B	  B��B	  B	B	B	
=B	PB	PB	\B	oB	oB	oB	uB	{B	�B	�B	�B	uB	VB	+B		7B	
=B	DB	JB	VB	�B	�B	#�B	%�B	+B	+B	,B	-B	/B	5?B	=qB	>wB	>wB	?}B	@�B	E�B	I�B	K�B	K�B	K�B	K�B	L�B	N�B	O�B	Q�B	S�B	VB	XB	YB	ZB	[#B	[#B	[#B	^5B	bNB	cTB	e`B	gmB	l�B	n�B	n�B	r�B	y�B	{�B	~�B	�B	�B	�B	�%B	�+B	�+B	�DB	�PB	�PB	�PB	�VB	�\B	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�9B	�?B	�LB	�RB	�RB	�jB	�}B	��B	��B	��B	B	B	ÖB	ÖB	ĜB	ŢB	ƨB	ƨB	ƨB	ƨB	ǮB	ǮB	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�/B	�5B	�5B	�;B	�HB	�ZB	�`B	�`B	�fB	�fB	�mB	�fB	�fB	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
DB
JB
JB
JB
JB
JB
JB
�B
�B
(�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.12 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140832                              AO  ARCAADJP                                                                    20181024140832    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140832  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140832  QCF$                G�O�G�O�G�O�0               