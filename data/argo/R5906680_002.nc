CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-07-17T10:00:48Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  pL   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �(   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �4   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �4   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �4   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �4   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �`   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �h   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �p   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210717100048  20210717100048  5906680 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  8780                            2B  A   NAVIS_A                         1308                            170425                          863 @ل���1   @ل�DDP�@+����o�dvz�G�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���C  C  C  C  C
  C  C�fC  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	�fD
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� DffDf� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�<�DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D��3D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�{@��HAp�A!p�AAp�Aap�A��RA��RA��RA��RA��RAиRA�RA�RB \)B\)B\)B\)B \)B(\)B0\)B8\)B@\)BH\)BP\)BX\)B`\)Bh\)Bp\)Bx\)B�.B�.B�.B���B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�aGB�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�aGB���C
C
C
C
C

C
C�pC
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
C.0�C0
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
CJ
CL
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
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	�)D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df)Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D�D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D��D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D�D���D��D�B�DÂ�D���D��D�B�DĂ�D���D��D�B�Dł�D���D��D�B�DƂ�D���D��D�B�Dǂ�D���D��D�B�DȂ�D���D��D�B�Dɂ�D���D��D�B�Dʂ�D���D��D�B�D˂�D���D��D�B�D̂�D���D��D�B�D͂�D���D��D�B�D΂�D���D��D�B�Dς�D���D��D�B�DЂ�D���D��D�B�Dт�D���D��D�B�D҂�D���D��D�?�Dӂ�D���D��D�B�DԂ�D���D��D�B�DՂ�D���D��D�B�Dւ�D���D��D�B�Dׂ�D���D��D�B�D؂�D���D��D�B�Dق�D���D��D�B�Dڂ�D���D��D�B�Dۂ�D���D��D�B�D܂�D���D��D�B�D݂�D���D��D�B�Dނ�D���D��D�B�D߂�D���D��D�B�D���D��D�D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D�D���D��D�B�D��D���D��D�B�D��D���D��D�B�D���D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D���D�B�D���D���D��D�B�D���D���D��D�B�D���D���D�{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AՁAՉ7AՉ7AՉ7AՇ+AՋDAՍPAՏ\AՍPAՏ\AՍPAՏ\AՏ\AՑhAՑhAՏ\AՏ\AՍPAՋDAՍPAՓuAՑhAՓuAՏ\AՍPAՇ+AՃAՅAՍPAՋDA�r�A�ZA�I�A�9XA�33A�33A�1'A�1'A�-A�(�A��A�=qA�dZAЍPA��A�z�A�AɅA�dZA���A���A�&�A��`Aƛ�A�?}A��TA��HA�bNADA��-A��HA�C�A��HA�1A�O�A�/A��`A� �A�|�A�9XA�33A��A��A�r�A�jA���A�l�A�$�A�O�A���A�A�Q�A��;A�VA�hsA���A�1A��A�n�A{t�Av�DApr�AghsA`�`A^ĜA]l�A\v�A[�PAW��AR�/AN�+AI�AG�AD^5AA��A@~�A=�7A9�PA7l�A6A5;dA3��A2��A2 �A0 �A/A.��A.A-G�A*I�A(�/A(-A%�wA%"�A%�A%
=A$�A$�A$��A$��A#�mA#�hA#p�A#XA#x�A#�A$�A$r�A#XA!"�A �AG�A�mAp�A�9A5?A�mA�
A(�AI�A33A��A��A�A��AE�A�FA�jA��A��A�A�mA��AA��A��A�A��AXA%AȴA7LA�yA�A{A��AdZAK�A+A�HA�uA  At�A`BAK�A��A�TA�^A��At�A�A�HA�\A��A��AE�A9XAbA/A
Q�A
v�A
��AVAA
�jA	t�A	VA	
=A��A�FAK�Al�A�hAp�AhsA�A�A�uA �A�jA5?AZAA��Ax�AhsA&�AhsA+A�DAbA/@�l�@��T@�x�@���@�+@���@���@��7@���@�I�@��;@�t�@���@��@�&�@�D@�A�@��@��@�ȴ@�`B@��/@�Z@@�dZ@�
=@���@�R@���@��/@�@�Q�@�S�@�{@��/@�9X@�@�+@��@�\@��@�/@��@�D@�bN@��@��y@��@��@�j@�@�9X@��@ߝ�@�t�@�;d@ޏ\@���@ݩ�@ݡ�@�x�@�&�@ܴ9@۶F@�@��@��`@���@֗�@�$�@��@���@�~�@�hs@��/@ЋD@�j@�Q�@�A�@�1@���@�"�@��@Ο�@́@̴9@̼j@���@���@�Ĝ@̣�@�(�@�|�@�S�@˕�@̛�@̴9@�bN@�ƨ@˕�@�S�@��H@�v�@Ɂ@ȣ�@���@�|�@���@Ƨ�@Ɵ�@�v�@��@�G�@Ĭ@�j@�(�@ÍP@���@�J@�J@��@�^5@��h@�O�@�G�@�X@�G�@���@�1'@�r�@��@��9@��@�|�@�S�@���@�v�@�G�@�r�@�b@��F@�@��+@�{@�@��7@�O�@�V@���@��D@�I�@�b@���@�@���@��-@��@���@���@���@�r�@�Q�@�(�@���@���@�$�@���@��@��9@�1'@�ƨ@���@�dZ@�+@���@�-@��^@��@�1'@���@��@��;@��
@�ƨ@�t�@�@�M�@��h@�7L@���@�A�@�1@���@���@�t�@�S�@�33@�"�@��@�n�@��@���@���@�bN@� �@�  @�t�@���@�n�@�=q@��@���@��@�p�@�O�@�7L@�&�@��@��@�z�@�I�@��@�ƨ@���@���@�K�@��@��!@��+@�ff@�E�@�J@�@�`B@��@�1'@�ƨ@�S�@���@��@�@��h@�/@��D@��;@�t�@�
=@��@�ȴ@���@�V@�@��@�G�@���@��@�j@��@��@�l�@���@��y@���@��!@�~�@�$�@��T@���@���@�@���@��h@�hs@�G�@�?}@�7L@�&�@��`@�j@��w@�;d@�+@���@�~�@���@�p�@��@���@��D@��@��j@��@��D@��D@�z�@�9X@���@��w@���@�33@��@��R@��!@���@�~�@�M�@��T@���@�x�@�O�@��@��/@�j@��@���@��w@��F@��@��P@��@�C�@��H@�ȴ@���@���@��R@��!@���@��\@�~�@�ff@�5?@���@��@���@�G�@�%@��@��j@���@�r�@�(�@�1@��;@���@�dZ@�C�@��@��H@��!@���@�n�@��#@�x�@�&�@���@���@��/@���@�Z@� �@�  @�;@|�@\)@K�@;d@+@~�y@~v�@~$�@}�T@}�@}?}@|�j@|�@|��@|(�@{��@{t�@{o@z~�@y�^@x�`@xb@w|�@wl�@w+@v�@vV@v{@u@t�@tz�@t�@s��@sC�@r��@r��@r^5@qx�@p�`@p�@p  @o|�@n�y@n�+@n5?@m�h@m?}@l�D@k��@k�@kdZ@kS�@j��@j=q@jJ@ix�@ihs@i�@hĜ@h�@h1'@g�w@g+@f�@f�+@f@e@e�@d�@dZ@c�
@c��@b��@a�^@a&�@a%@`b@_;d@_
=@^�y@^��@^V@^$�@]�@]/@]V@\�/@\�@\�D@\9X@[��@[S�@[@Z�!@Z^5@Z-@Y��@Yhs@Y�@X�`@X�@X  @X  @W�@W��@W�w@W��@W;d@V��@V��@Vff@U��@UV@T��@Tz�@Sƨ@S33@R�@R��@Rn�@Rn�@Rn�@R^5@Q�#@Qx�@Q�7@Q�^@Qx�@PbN@O�;@O�;@O|�@Nv�@M��@Mp�@MO�@M�@L��@L��@L�j@L��@LI�@K��@Kt�@K"�@Ko@J�H@J=q@I��@I�7@H�`@H��@Hr�@H1'@G�;@G�@H  @G��@GK�@G�@F�y@F�+@F$�@F{@E�-@E��@E�@D�j@Dz�@C��@B��@BJ@Ax�@@��@@bN@@b@?��@>�R@>v�@>V@>5?@>5?@>$�@>{@=��@=�@=?}@=�@=V@<��@<�@<�j@<��@<z�@<j@<Z@;��@:��@:�!@:��@:=q@9�^@9�@8�@8bN@8A�@8b@7��@7|�@7\)@7
=@6��@6V@5��@5p�@4�D@4�@3S�@3o@2��@2-@1�@1��@1x�@1&�@0�@0b@/l�@/+@/�@.�R@.@-O�@,��@,�@+�
@+��@*��@*n�@)��@)&�@(�`@(r�@( �@'�;@'�@'��@'�P@'l�@'l�@'\)@&�y@&V@&@%�h@%?}@%?}@%V@$�D@#��@$(�@$(�@$(�@$�@#�m@#C�@#33@#o@"�H@"n�@"�@"J@!��@!�@!�^@!�7@!7L@!%@ �`@ Ĝ@ ��@ bN@ A�@ A�@ Q�@ A�@ A�@ 1'@�@l�@
=@�@��@��@v�@V@5?@@�@�T@@�h@�@?}@��@�j@��@�D@z�@I�@��@�F@C�@��@�@�#@��@hs@G�@&�@&�@�@�`@Ĝ@�9@�9@��@��@�u@r�@A�@ �@ �@ �@�@�;@�w@l�@\)@\)@K�@��@�y@��@v�@E�@@�-@�@p�@`B@O�@O�@/@��@�/@�D@j@I�@(�@�@��@�m@ƨ@��@dZ@o@��@��@��@�!@�\@M�@�@�#@�#@��@7L@��@Ĝ@��@�@A�@ �@b@  @�@�;@�w@�@�P@l�@�@�y@��@�+@v�@V@5?@$�@$�@�T@��@�h@�@�@�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AՁAՉ7AՉ7AՉ7AՇ+AՋDAՍPAՏ\AՍPAՏ\AՍPAՏ\AՏ\AՑhAՑhAՏ\AՏ\AՍPAՋDAՍPAՓuAՑhAՓuAՏ\AՍPAՇ+AՃAՅAՍPAՋDA�r�A�ZA�I�A�9XA�33A�33A�1'A�1'A�-A�(�A��A�=qA�dZAЍPA��A�z�A�AɅA�dZA���A���A�&�A��`Aƛ�A�?}A��TA��HA�bNADA��-A��HA�C�A��HA�1A�O�A�/A��`A� �A�|�A�9XA�33A��A��A�r�A�jA���A�l�A�$�A�O�A���A�A�Q�A��;A�VA�hsA���A�1A��A�n�A{t�Av�DApr�AghsA`�`A^ĜA]l�A\v�A[�PAW��AR�/AN�+AI�AG�AD^5AA��A@~�A=�7A9�PA7l�A6A5;dA3��A2��A2 �A0 �A/A.��A.A-G�A*I�A(�/A(-A%�wA%"�A%�A%
=A$�A$�A$��A$��A#�mA#�hA#p�A#XA#x�A#�A$�A$r�A#XA!"�A �AG�A�mAp�A�9A5?A�mA�
A(�AI�A33A��A��A�A��AE�A�FA�jA��A��A�A�mA��AA��A��A�A��AXA%AȴA7LA�yA�A{A��AdZAK�A+A�HA�uA  At�A`BAK�A��A�TA�^A��At�A�A�HA�\A��A��AE�A9XAbA/A
Q�A
v�A
��AVAA
�jA	t�A	VA	
=A��A�FAK�Al�A�hAp�AhsA�A�A�uA �A�jA5?AZAA��Ax�AhsA&�AhsA+A�DAbA/@�l�@��T@�x�@���@�+@���@���@��7@���@�I�@��;@�t�@���@��@�&�@�D@�A�@��@��@�ȴ@�`B@��/@�Z@@�dZ@�
=@���@�R@���@��/@�@�Q�@�S�@�{@��/@�9X@�@�+@��@�\@��@�/@��@�D@�bN@��@��y@��@��@�j@�@�9X@��@ߝ�@�t�@�;d@ޏ\@���@ݩ�@ݡ�@�x�@�&�@ܴ9@۶F@�@��@��`@���@֗�@�$�@��@���@�~�@�hs@��/@ЋD@�j@�Q�@�A�@�1@���@�"�@��@Ο�@́@̴9@̼j@���@���@�Ĝ@̣�@�(�@�|�@�S�@˕�@̛�@̴9@�bN@�ƨ@˕�@�S�@��H@�v�@Ɂ@ȣ�@���@�|�@���@Ƨ�@Ɵ�@�v�@��@�G�@Ĭ@�j@�(�@ÍP@���@�J@�J@��@�^5@��h@�O�@�G�@�X@�G�@���@�1'@�r�@��@��9@��@�|�@�S�@���@�v�@�G�@�r�@�b@��F@�@��+@�{@�@��7@�O�@�V@���@��D@�I�@�b@���@�@���@��-@��@���@���@���@�r�@�Q�@�(�@���@���@�$�@���@��@��9@�1'@�ƨ@���@�dZ@�+@���@�-@��^@��@�1'@���@��@��;@��
@�ƨ@�t�@�@�M�@��h@�7L@���@�A�@�1@���@���@�t�@�S�@�33@�"�@��@�n�@��@���@���@�bN@� �@�  @�t�@���@�n�@�=q@��@���@��@�p�@�O�@�7L@�&�@��@��@�z�@�I�@��@�ƨ@���@���@�K�@��@��!@��+@�ff@�E�@�J@�@�`B@��@�1'@�ƨ@�S�@���@��@�@��h@�/@��D@��;@�t�@�
=@��@�ȴ@���@�V@�@��@�G�@���@��@�j@��@��@�l�@���@��y@���@��!@�~�@�$�@��T@���@���@�@���@��h@�hs@�G�@�?}@�7L@�&�@��`@�j@��w@�;d@�+@���@�~�@���@�p�@��@���@��D@��@��j@��@��D@��D@�z�@�9X@���@��w@���@�33@��@��R@��!@���@�~�@�M�@��T@���@�x�@�O�@��@��/@�j@��@���@��w@��F@��@��P@��@�C�@��H@�ȴ@���@���@��R@��!@���@��\@�~�@�ff@�5?@���@��@���@�G�@�%@��@��j@���@�r�@�(�@�1@��;@���@�dZ@�C�@��@��H@��!@���@�n�@��#@�x�@�&�@���@���@��/@���@�Z@� �@�  @�;@|�@\)@K�@;d@+@~�y@~v�@~$�@}�T@}�@}?}@|�j@|�@|��@|(�@{��@{t�@{o@z~�@y�^@x�`@xb@w|�@wl�@w+@v�@vV@v{@u@t�@tz�@t�@s��@sC�@r��@r��@r^5@qx�@p�`@p�@p  @o|�@n�y@n�+@n5?@m�h@m?}@l�D@k��@k�@kdZ@kS�@j��@j=q@jJ@ix�@ihs@i�@hĜ@h�@h1'@g�w@g+@f�@f�+@f@e@e�@d�@dZ@c�
@c��@b��@a�^@a&�@a%@`b@_;d@_
=@^�y@^��@^V@^$�@]�@]/@]V@\�/@\�@\�D@\9X@[��@[S�@[@Z�!@Z^5@Z-@Y��@Yhs@Y�@X�`@X�@X  @X  @W�@W��@W�w@W��@W;d@V��@V��@Vff@U��@UV@T��@Tz�@Sƨ@S33@R�@R��@Rn�@Rn�@Rn�@R^5@Q�#@Qx�@Q�7@Q�^@Qx�@PbN@O�;@O�;@O|�@Nv�@M��@Mp�@MO�@M�@L��@L��@L�j@L��@LI�@K��@Kt�@K"�@Ko@J�H@J=q@I��@I�7@H�`@H��@Hr�@H1'@G�;@G�@H  @G��@GK�@G�@F�y@F�+@F$�@F{@E�-@E��@E�@D�j@Dz�@C��@B��@BJ@Ax�@@��@@bN@@b@?��@>�R@>v�@>V@>5?@>5?@>$�@>{@=��@=�@=?}@=�@=V@<��@<�@<�j@<��@<z�@<j@<Z@;��@:��@:�!@:��@:=q@9�^@9�@8�@8bN@8A�@8b@7��@7|�@7\)@7
=@6��@6V@5��@5p�@4�D@4�@3S�@3o@2��@2-@1�@1��@1x�@1&�@0�@0b@/l�@/+@/�@.�R@.@-O�@,��@,�@+�
@+��@*��@*n�@)��@)&�@(�`@(r�@( �@'�;@'�@'��@'�P@'l�@'l�@'\)@&�y@&V@&@%�h@%?}@%?}@%V@$�D@#��@$(�@$(�@$(�@$�@#�m@#C�@#33@#o@"�H@"n�@"�@"J@!��@!�@!�^@!�7@!7L@!%@ �`@ Ĝ@ ��@ bN@ A�@ A�@ Q�@ A�@ A�@ 1'@�@l�@
=@�@��@��@v�@V@5?@@�@�T@@�h@�@?}@��@�j@��@�D@z�@I�@��@�F@C�@��@�@�#@��@hs@G�@&�@&�@�@�`@Ĝ@�9@�9@��@��@�u@r�@A�@ �@ �@ �@�@�;@�w@l�@\)@\)@K�@��@�y@��@v�@E�@@�-@�@p�@`B@O�@O�@/@��@�/@�D@j@I�@(�@�@��@�m@ƨ@��@dZ@o@��@��@��@�!@�\@M�@�@�#@�#@��@7L@��@Ĝ@��@�@A�@ �@b@  @�@�;@�w@�@�P@l�@�@�y@��@�+@v�@V@5?@$�@$�@�T@��@�h@�@�@�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
K�B
L�B
L�B
L�B
M�B
L�B
L�B
N�B
F�B
7LB
/B
6FB
L�B
\)B
^5B
ffB
� B
��B
�B
ƨB
��BDBbBO�BYBaHB}�Bx�Br�Bm�Bp�Bu�B�VB�{B��B��B�=Bm�BjBffB\)B9XB�BB
�B
�NB
��B
��B
�wB
��B
�7B
s�B
]/B
�B
1B	�B	��B	�'B	�B	cTB	W
B	R�B	O�B	O�B	D�B	5?B	'�B	�B	�B	uB	oB	PB	JB	1B	1B		7B	DB	uB	�B	�B	%�B	,B	33B	6FB	7LB	?}B	B�B	E�B	^5B	r�B	w�B	}�B	�B	�B	�%B	�JB	��B	��B	��B	�B	�-B	��B	��B	�B	�HB	��B	�qB	�^B	�dB	�^B	B	ÖB	ĜB	�
B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B
PB
&�B
1'B
49B
;dB
<jB
:^B
:^B
8RB
5?B
49B
33B
<jB
=qB
;dB
9XB
6FB
49B
49B
33B
2-B
2-B
2-B
-B
,B
-B
+B
&�B
&�B
(�B
+B
,B
,B
.B
6FB
;dB
7LB
8RB
;dB
49B
.B
0!B
33B
@�B
B�B
A�B
;dB
7LB
7LB
5?B
/B
+B
/B
5?B
5?B
5?B
5?B
49B
5?B
1'B
'�B
!�B
&�B
'�B
#�B
$�B
-B
+B
1'B
2-B
.B
+B
%�B
�B
�B
�B
�B
�B
�B
{B
uB
oB
hB
\B
\B
VB
PB
PB
PB
PB
VB
PB
PB
PB
JB
JB
DB
PB
PB
JB
JB

=B
1B
+B
1B
B
B
  B
  B	��B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�fB	�sB	�B	�B	�B	�B	�B	�yB	�sB	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
%B
%B
%B
+B
1B
1B
1B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B

=B
JB
DB
JB
JB
JB
JB
PB
VB
PB
VB
VB
VB
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
bB
bB
\B
bB
bB
bB
hB
bB
hB
hB
hB
hB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
"�B
"�B
!�B
"�B
!�B
 �B
 �B
!�B
!�B
!�B
"�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
&�B
&�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
)�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
,B
.B
/B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
49B
49B
49B
33B
49B
33B
33B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
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
7LB
7LB
7LB
8RB
8RB
9XB
:^B
:^B
:^B
:^B
;dB
:^B
:^B
9XB
9XB
:^B
:^B
;dB
;dB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
?}B
@�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
D�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
J�B
I�B
I�B
I�B
H�B
I�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
K�B
K�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
S�B
S�B
S�B
T�B
VB
W
B
W
B
W
B
W
B
VB
VB
VB
VB
VB
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
`BB
aHB
aHB
aHB
aHB
bNB
aHB
bNB
aHB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
gmB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
jB
jB
k�B
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
K�B
L�B
L�B
L�B
M�B
L�B
L�B
N�B
F�B
7LB
/B
6FB
L�B
\)B
^5B
ffB
� B
��B
�B
ƨB
��BDBbBO�BYBaHB}�Bx�Br�Bm�Bp�Bu�B�VB�{B��B��B�=Bm�BjBffB\)B9XB�BB
�B
�NB
��B
��B
�wB
��B
�7B
s�B
]/B
�B
1B	�B	��B	�'B	�B	cTB	W
B	R�B	O�B	O�B	D�B	5?B	'�B	�B	�B	uB	oB	PB	JB	1B	1B		7B	DB	uB	�B	�B	%�B	,B	33B	6FB	7LB	?}B	B�B	E�B	^5B	r�B	w�B	}�B	�B	�B	�%B	�JB	��B	��B	��B	�B	�-B	��B	��B	�B	�HB	��B	�qB	�^B	�dB	�^B	B	ÖB	ĜB	�
B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B
PB
&�B
1'B
49B
;dB
<jB
:^B
:^B
8RB
5?B
49B
33B
<jB
=qB
;dB
9XB
6FB
49B
49B
33B
2-B
2-B
2-B
-B
,B
-B
+B
&�B
&�B
(�B
+B
,B
,B
.B
6FB
;dB
7LB
8RB
;dB
49B
.B
0!B
33B
@�B
B�B
A�B
;dB
7LB
7LB
5?B
/B
+B
/B
5?B
5?B
5?B
5?B
49B
5?B
1'B
'�B
!�B
&�B
'�B
#�B
$�B
-B
+B
1'B
2-B
.B
+B
%�B
�B
�B
�B
�B
�B
�B
{B
uB
oB
hB
\B
\B
VB
PB
PB
PB
PB
VB
PB
PB
PB
JB
JB
DB
PB
PB
JB
JB

=B
1B
+B
1B
B
B
  B
  B	��B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�fB	�sB	�B	�B	�B	�B	�B	�yB	�sB	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
%B
%B
%B
+B
1B
1B
1B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B

=B
JB
DB
JB
JB
JB
JB
PB
VB
PB
VB
VB
VB
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
bB
bB
\B
bB
bB
bB
hB
bB
hB
hB
hB
hB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
"�B
"�B
!�B
"�B
!�B
 �B
 �B
!�B
!�B
!�B
"�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
&�B
&�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
)�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
,B
.B
/B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
49B
49B
49B
33B
49B
33B
33B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
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
7LB
7LB
7LB
8RB
8RB
9XB
:^B
:^B
:^B
:^B
;dB
:^B
:^B
9XB
9XB
:^B
:^B
;dB
;dB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
?}B
@�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
D�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
J�B
I�B
I�B
I�B
H�B
I�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
K�B
K�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
S�B
S�B
S�B
T�B
VB
W
B
W
B
W
B
W
B
VB
VB
VB
VB
VB
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
`BB
aHB
aHB
aHB
aHB
bNB
aHB
bNB
aHB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
gmB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
jB
jB
k�B
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.09 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210717100048                              AO  ARCAADJP                                                                    20210717100048    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210717100048  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210717100048  QCF$                G�O�G�O�G�O�0               