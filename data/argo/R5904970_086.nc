CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:15:15Z creation      
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
resolution        =���   axis      Z           9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ^    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        `   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   h(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        j0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        rP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   zp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        |x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024141515  20181024141515  5904970 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               VA   AO  6785                            2B  A   APEX                            7726                            111215                          846 @�ʤ��n1   @�ʥhK��@3�9Xb�cݙ����1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      VB   B   B   @�33@�  A   A   A@  A`  A���A�  A�  A���A�  A�  A�  A�  A�33BffB��B��B   B(  B0  B8  B@  BH  BP  BXffB`ffBg��Bo��A�  A�  A���A�  A�  A�  A�  A�33BffB��B��B   B(  B0  B8  B@  BH  BP  BXffB`ffBg��Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C�fC  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D;��D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^fD^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw� Dy��D�33D�
1111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�{@��HAp�A!p�AAp�Aap�A��A��RA��RA��A��RAиRA�RA�RA��BB��B��B \)B(\)B0\)B8\)B@\)BH\)BP\)BXB`Bg��Bo��A��RA��RA��A��RAиRA�RA�RA��BB��B��B \)B(\)B0\)B8\)B@\)BH\)BP\)BXB`Bg��Bo��Bx\)B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�aGB�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B���C�pC
C
C
C

C
C
C
C
C
C
C
C
C
C
C 
C"
C$
C&
C(
C*
C,
C.
C0
C2
C4
C6
C8
C:
C<
C>
C@
CB
CD
CF
CH
CI�pCL
CN
CP
CR
CT
CV
CX
CZ
C\
C^
C`
Cb
Cd
Cf
Ch
Cj
Cl
Cn
Cp
Cr
Ct
Cv
Cx
Cz
C|
C~
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C���C��C��C��C��C�RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D;�]D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ�)DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^)D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dw��Dy�HD�6D���1111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�G�A�E�A�C�A�E�A�A�AߓuA�E�A�1'A�bA��A���Aް!Aޥ�Aއ+A� �A��A��A��A�ȴA�=qA�  A�r�Aɕ�A��;A� �A�ĜA��A�  A���A�1'A�bA��A���Aް!Aޥ�Aއ+A� �A��A��A��A�ȴA�=qA�  A�r�Aɕ�A��;A� �A�ĜA��A�  A���A�|�A��HA��7A�(�A�v�A���A�A�A���A�&�A�`BA��-A�x�A��A���A���A�A��FA���A�=qA��A��A���A��PA��/A�
=A�v�A�K�A�A�A��hA�jA��;A��\A��FA�
=A�v�A��TA���A�VA���A�XA�%A�XA��A��7A�ĜA� �A��A��PA��mA� �A�
=A�ffA���A�;dA��RA��yA�I�A~A�A|JAz�HAzAv�DArQ�ApM�Am�FAk33Ag�^Ae�mAe��Ac��A`�A[XAW�AV��AV1AU�-AT��AT��AT9XASAS�^AS|�ARQ�AQ\)AP9XAN�AM��AM�ALI�AKl�AJ��AI/AG;dAFJAE/ACx�AA�A@��A?�PA:��A8n�A5XA2��A1�A/��A-��A*��A'�A&v�A%��A%��A%�A%x�A%�A#�wA!G�A�yA��A;dA�A�!A�uAv�AJA/AZA�!A\)A�A��A$�AhsAffAI�A�^Ax�A��AA5?A9XA
�A	A��AbNAXAZAl�A�;A�jA|�AA @�n�@��@���@��/@���@���@��m@�V@���@�%@���@�?}@���@�r�@�+@�-@���@��H@���@�O�@�(�@�;d@�@�7@�7@�X@���@�  @��@�;d@�K�@�w@㕁@�o@���@⟾@�$�@��@�j@��D@�9X@���@�G�@ܴ9@۶F@�\)@��@�@��@�{@�S�@Ь@ϝ�@�;d@�v�@�-@�J@�7L@���@�1'@��;@˅@���@�n�@���@ɡ�@�x�@��@�Q�@��;@�|�@�"�@���@�M�@�J@�x�@�1'@�I�@�(�@���@���@��D@�9X@��@���@��@��@��u@���@�G�@�&�@�bN@� �@�  @��
@��@�l�@��H@�ff@�`B@�I�@���@���@���@�x�@�hs@�G�@�&�@�V@��@�j@�1@��w@��@�dZ@�@��@�x�@�X@�O�@�?}@��`@��@���@�^5@�{@���@��h@�`B@��@�I�@�S�@�@��H@�ȴ@���@���@��+@�V@���@�p�@�`B@�O�@�&�@���@�bN@�9X@�1'@��m@���@���@���@�^5@���@��^@�O�@��9@��m@�l�@��@�K�@�l�@�dZ@�t�@��P@�|�@�l�@�|�@�dZ@���@�v�@�ff@�^5@�E�@�5?@�{@���@��@��T@���@��^@��h@���@�bN@��@���@���@�33@���@�{@���@��@��9@� �@�1@��m@��P@�33@��@���@�5?@�J@��@�@��@���@��@�x�@���@�`B@��j@���@��@��@� �@���@���@�K�@�o@��R@��+@�~�@�5?@���@��@��@��#@���@�@���@�?}@��@�1'@�b@�1@���@��
@���@�|�@�33@�@��y@��R@�ff@���@���@�A�@���@��@�dZ@�;d@���@���@��+@��@��#@���@���@���@��^@���@��@�7L@��j@���@�bN@�A�@��;@���@�K�@�;d@�33@�"�@�ff@��@��7@�hs@�?}@�7L@�/@��@���@�9X@�1@��m@�ƨ@��w@��F@��@�;d@��!@�ff@��@�hs@��/@���@�Z@���@��@���@���@��P@�|�@�t�@�dZ@�\)@��@���@��@��@��@��y@�ȴ@�ff@�+�@n�@a:�1111111111111111111111111114141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�G�A�E�A�C�A�E�A�A�AߓuA�E�A�1'A�bA��A���Aް!Aޥ�Aއ+A� �A��A��A��A�ȴA�=qA�  A�r�Aɕ�A��;A� �A�ĜA��A�  A���A�1'A�bA��A���Aް!Aޥ�Aއ+A� �A��A��A��A�ȴA�=qA�  A�r�Aɕ�A��;A� �A�ĜA��A�  A���A�|�A��HA��7A�(�A�v�A���A�A�A���A�&�A�`BA��-A�x�A��A���A���A�A��FA���A�=qA��A��A���A��PA��/A�
=A�v�A�K�A�A�A��hA�jA��;A��\A��FA�
=A�v�A��TA���A�VA���A�XA�%A�XA��A��7A�ĜA� �A��A��PA��mA� �A�
=A�ffA���A�;dA��RA��yA�I�A~A�A|JAz�HAzAv�DArQ�ApM�Am�FAk33Ag�^Ae�mAe��Ac��A`�A[XAW�AV��AV1AU�-AT��AT��AT9XASAS�^AS|�ARQ�AQ\)AP9XAN�AM��AM�ALI�AKl�AJ��AI/AG;dAFJAE/ACx�AA�A@��A?�PA:��A8n�A5XA2��A1�A/��A-��A*��A'�A&v�A%��A%��A%�A%x�A%�A#�wA!G�A�yA��A;dA�A�!A�uAv�AJA/AZA�!A\)A�A��A$�AhsAffAI�A�^Ax�A��AA5?A9XA
�A	A��AbNAXAZAl�A�;A�jA|�AA @�n�@��@���@��/@���@���@��m@�V@���@�%@���@�?}@���@�r�@�+@�-@���@��H@���@�O�@�(�@�;d@�@�7@�7@�X@���@�  @��@�;d@�K�@�w@㕁@�o@���@⟾@�$�@��@�j@��D@�9X@���@�G�@ܴ9@۶F@�\)@��@�@��@�{@�S�@Ь@ϝ�@�;d@�v�@�-@�J@�7L@���@�1'@��;@˅@���@�n�@���@ɡ�@�x�@��@�Q�@��;@�|�@�"�@���@�M�@�J@�x�@�1'@�I�@�(�@���@���@��D@�9X@��@���@��@��@��u@���@�G�@�&�@�bN@� �@�  @��
@��@�l�@��H@�ff@�`B@�I�@���@���@���@�x�@�hs@�G�@�&�@�V@��@�j@�1@��w@��@�dZ@�@��@�x�@�X@�O�@�?}@��`@��@���@�^5@�{@���@��h@�`B@��@�I�@�S�@�@��H@�ȴ@���@���@��+@�V@���@�p�@�`B@�O�@�&�@���@�bN@�9X@�1'@��m@���@���@���@�^5@���@��^@�O�@��9@��m@�l�@��@�K�@�l�@�dZ@�t�@��P@�|�@�l�@�|�@�dZ@���@�v�@�ff@�^5@�E�@�5?@�{@���@��@��T@���@��^@��h@���@�bN@��@���@���@�33@���@�{@���@��@��9@� �@�1@��m@��P@�33@��@���@�5?@�J@��@�@��@���@��@�x�@���@�`B@��j@���@��@��@� �@���@���@�K�@�o@��R@��+@�~�@�5?@���@��@��@��#@���@�@���@�?}@��@�1'@�b@�1@���@��
@���@�|�@�33@�@��y@��R@�ff@���@���@�A�@���@��@�dZ@�;d@���@���@��+@��@��#@���@���@���@��^@���@��@�7L@��j@���@�bN@�A�@��;@���@�K�@�;d@�33@�"�@�ff@��@��7@�hs@�?}@�7L@�/@��@���@�9X@�1@��m@�ƨ@��w@��F@��@�;d@��!@�ff@��@�hs@��/@���@�Z@���@��@���@���@��P@�|�@�t�@�dZ@�\)@��@���@��@��@��@��y@�ȴ@�ff@�+�@n�@a:�1111111111111111111111111114141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B|�Bx�Bw�Bu�Bs�Bq�Br�Br�Bo�BjB��B�B��B�B�B�B�B�BB�B��BJBhB�B"�Bw�Bu�Bs�Bq�Br�Br�Bo�BjB��B�B��B�B�B�B�B�BB�B��BJBhB�B%�BH�BM�BN�BM�BQ�BQ�BN�BM�BL�BK�BL�BH�BD�BA�BD�B@�BD�BG�B?}B<jB:^B49B#�B�BuB�BuBB�B�B�sB�)B��BB�RB�B�VB�B|�Bw�Bs�BiyBbNB]/BQ�B �B
��B
�B
�^B
�B
��B
M�B
0!B
�B
oB
'�B
-B
0!B
"�B
�B
�B
B	�B	ɺB	�jB	�3B	��B	��B	��B	�hB	~�B	ffB	N�B	H�B	D�B	B�B	<jB	:^B	9XB	8RB	8RB	7LB	49B	.B	)�B	!�B	�B	�B	�B	\B	JB	%B��B��B�B�B�`B�;B�/B��BƨBĜB�LB�3B�B��B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B�hB�PB�JB�PB�DB�uB�uB��B�{B�uB�{B�VB�bB�VB�1B�+B�DB�B}�Bz�Bu�Bo�Bm�BhsBffBbNB_;BZBW
BW
BZB[#BjBm�Bl�Bo�Bv�Bx�B�B� B� B}�Bz�Bw�Bv�Bs�Bq�Bp�Bq�Br�Bu�Bz�B}�B�7B�PB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�FB�XB�dB�qB�}B��B��BÖBȴB��B��B��B��B��B��B��B��B��B��B�
B�B�B�#B�)B�B�B�B�B�/B�;B�HB�/B�)B�B�B��B��B�B�;B�`B�yB��B	  B	B	B	B	B	%B		7B	
=B	DB	bB	uB	�B	�B	�B	�B	�B	#�B	$�B	%�B	+B	.B	1'B	6FB	7LB	:^B	?}B	@�B	@�B	@�B	@�B	B�B	G�B	Q�B	T�B	W
B	XB	ZB	[#B	^5B	dZB	ffB	iyB	jB	k�B	l�B	l�B	l�B	n�B	u�B	x�B	x�B	y�B	y�B	{�B	� B	�B	�B	�B	�B	�B	�=B	�DB	�PB	�PB	�VB	�bB	�{B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�?B	�RB	�RB	�XB	�XB	�XB	�XB	�^B	�^B	�^B	�^B	�dB	�dB	�dB	�wB	��B	ÖB	ÖB	ÖB	ÖB	ĜB	ÖB	ÖB	ÖB	ĜB	ĜB	ĜB	ĜB	ĜB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	�B	�5B	�HB	�ZB	�fB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
1B
1B
1B
1B
	7B
	7B

=B

=B
DB
DB
DB
DB
JB
DB
JB
PB
VB
\B
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
&�B
/O1111111111111111111111111114141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B|�Bx�Bw�Bu�Bs�Bq�Br�Br�Bo�BjB��B�B��B�B�B�B�B�BB�B��BJBhB�B"�Bw�Bu�Bs�Bq�Br�Br�Bo�BjB��B�B��B�B�B�B�B�BB�B��BJBhB�B%�BH�BM�BN�BM�BQ�BQ�BN�BM�BL�BK�BL�BH�BD�BA�BD�B@�BD�BG�B?}B<jB:^B49B#�B�BuB�BuBB�B�B�sB�)B��BB�RB�B�VB�B|�Bw�Bs�BiyBbNB]/BQ�B �B
��B
�B
�^B
�B
��B
M�B
0!B
�B
oB
'�B
-B
0!B
"�B
�B
�B
B	�B	ɺB	�jB	�3B	��B	��B	��B	�hB	~�B	ffB	N�B	H�B	D�B	B�B	<jB	:^B	9XB	8RB	8RB	7LB	49B	.B	)�B	!�B	�B	�B	�B	\B	JB	%B��B��B�B�B�`B�;B�/B��BƨBĜB�LB�3B�B��B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B�hB�PB�JB�PB�DB�uB�uB��B�{B�uB�{B�VB�bB�VB�1B�+B�DB�B}�Bz�Bu�Bo�Bm�BhsBffBbNB_;BZBW
BW
BZB[#BjBm�Bl�Bo�Bv�Bx�B�B� B� B}�Bz�Bw�Bv�Bs�Bq�Bp�Bq�Br�Bu�Bz�B}�B�7B�PB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�FB�XB�dB�qB�}B��B��BÖBȴB��B��B��B��B��B��B��B��B��B��B�
B�B�B�#B�)B�B�B�B�B�/B�;B�HB�/B�)B�B�B��B��B�B�;B�`B�yB��B	  B	B	B	B	B	%B		7B	
=B	DB	bB	uB	�B	�B	�B	�B	�B	#�B	$�B	%�B	+B	.B	1'B	6FB	7LB	:^B	?}B	@�B	@�B	@�B	@�B	B�B	G�B	Q�B	T�B	W
B	XB	ZB	[#B	^5B	dZB	ffB	iyB	jB	k�B	l�B	l�B	l�B	n�B	u�B	x�B	x�B	y�B	y�B	{�B	� B	�B	�B	�B	�B	�B	�=B	�DB	�PB	�PB	�VB	�bB	�{B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�?B	�RB	�RB	�XB	�XB	�XB	�XB	�^B	�^B	�^B	�^B	�dB	�dB	�dB	�wB	��B	ÖB	ÖB	ÖB	ÖB	ĜB	ÖB	ÖB	ÖB	ĜB	ĜB	ĜB	ĜB	ĜB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	�B	�5B	�HB	�ZB	�fB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
1B
1B
1B
1B
	7B
	7B

=B

=B
DB
DB
DB
DB
JB
DB
JB
PB
VB
\B
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
&�B
/O1111111111111111111111111114141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.09 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024141515                              AO  ARCAADJP                                                                    20181024141515    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024141515  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024141515  QCF$                G�O�G�O�G�O�4000            