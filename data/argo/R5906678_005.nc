CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-07-20T10:01:06Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݀   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݰ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20210720100106  20210720100106  5906678 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  8777                            2B  A   NAVIS_A                         1305                            170425                          863 @مY�7��1   @مZDDQ0@(�I�^�d�vȴ9X1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         A   A   A   @�ff@���A   A   A@  A`  A�  A�  A�  A�33A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB�33B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B���B�  B�  B�  C   C  C  C  C  C
  C  C�C�C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�3D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�L�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�G�@��Ap�A!p�AAp�Aap�A��RA��RA��RA��A��RAхA�RA�RB \)B\)B\)B\)B \)B(\)B0\)B8\)B@\)BH\)BP\)BX\)B`\)Bh\)Bp\)Bx\)B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�aGB��zB�aGB���B�.B�.B�.B�.B�.B�.B�.B�aGB�.B�.B���B���B�.B�.B�.C 
C
C
C
C
C

C
C0�C0�C�pC
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
C80�C:0�C<
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
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�FD���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D�D���D��D�B�DÂ�D���D��D�B�DĂ�D���D��D�B�Dł�D���D��D�B�DƂ�D���D��D�B�Dǂ�D���D��D�B�DȂ�D���D��D�B�Dɂ�D���D��D�B�Dʂ�D���D��D�B�D˂�D���D��D�B�D̂�D���D��D�B�D͂�D���D��D�B�D΂�D���D�D�B�Dς�D���D��D�B�DЂ�D���D��D�B�Dт�D���D��D�B�D҂�D���D��D�B�Dӂ�D���D��D�B�DԂ�D���D��D�B�DՂ�D���D��D�B�Dւ�D���D��D�B�Dׂ�D���D��D�B�D؂�D���D��D�B�Dق�D���D��D�B�Dڂ�D���D��D�B�Dۂ�D���D��D�B�D܂�D���D��D�B�D݂�D���D��D�B�Dނ�D���D��D�B�D߂�D���D��D�B�D���D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D���D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D�D��D�B�D��D���D��D�B�D���D���D��D�?�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D�D�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�K�A�Q�A�S�A�^5A�`BA�`BA�`BA�dZA�ffA�ffA�\)A�XA�VA�ZA�\)A�ZA�\)A�O�A�C�A�O�A�O�A�K�A�C�A�&�A�1A�1A�1A�  A�  A���A���A��mA���A״9Aם�Aח�Aו�Aׇ+A�ffA�;dA�bAִ9A�t�A�ƨA�A��A�n�A���A�C�A�bA�ZA�"�AÅA�A¥�A�bA���A���A��PA���A�p�A��A��A���A�"�A�O�A�JA���A��#A�I�A��\A��hA���A�"�A�x�A�+A���A��A�  A���A�7LA��yA�$�A�O�A�E�A�p�A�VAy��Ay7LAx�Aw�PAvQ�AuXAp�Ae\)A`��AZ�AT^5ANz�AE7LAB�/A@z�A?A>�9A=%A;&�A9XA7�#A7�A6�A6=qA5VA4�A4Q�A3�A3A1t�A,�yA+�#A+�hA*�`A*�jA*�A*�+A*��A*v�A)+A'A'
=A&n�A%"�A$ĜA$��A$^5A$�A#��A"��A!S�A ��A��A�A�7A�RA9XA��A�A�A�PAK�AA��AVAE�A=qA1'AJA��A�DA��A��A�A$�A;dA��A�DAA�AA&�A�yAZA�hA/A%A��A�uA^5AffA{AƨAdZA�jA�+A{A�7A�AȴA��A�A?}AA
��A
��A
�A
��A
jA
1'A
{A	��A	l�A�DA9XA �AƨA&�A�+AQ�A1A��A��AdZAĜA��AbNA �A�A\)A��A�9A��A��A�\AjA  Al�AK�A/A �A ^5A 1'A �@��F@�+@���@�@�@�O�@�Ĝ@��@��P@�
=@�E�@���@���@��h@��@��@�j@�b@��w@��@���@�l�@�;d@���@��@��@��m@�"�@�J@�X@�Z@�^5@��-@�hs@��/@�r�@�bN@�w@�S�@���@�M�@陚@�/@�j@�(�@�ƨ@�C�@��H@�=q@�J@�-@��@�bN@�|�@�V@�h@��@�z�@��;@߅@�S�@�+@�n�@��`@ܣ�@܋D@�z�@�bN@�  @��y@ٲ-@�1'@�S�@���@֗�@�^5@��@թ�@�hs@Լj@�Q�@ӶF@ҏ\@��@�Ĝ@�j@�Q�@�(�@Ϯ@�dZ@�33@��@Χ�@���@�7L@���@�bN@��@���@���@ˮ@˅@�t�@�K�@�
=@ʸR@�~�@�V@�$�@��@�@�hs@�%@ț�@�1'@���@ǝ�@�33@ƸR@ř�@�?}@��`@ļj@ģ�@ēu@ċD@ă@ă@�z�@�r�@�1'@Å@�\)@�ȴ@��@��h@�G�@��@��D@���@�t�@��H@�^5@�^5@�-@��@�@��@�G�@�Ĝ@��w@�+@��H@��\@��@���@��w@��w@��w@��F@��P@�;d@���@���@�x�@�&�@�j@�b@���@��F@�l�@��@��\@�~�@�~�@�^5@�$�@��@���@���@��@�7L@���@���@��@�(�@��w@�K�@�@�~�@�{@�V@��9@�A�@�o@�n�@�=q@���@��D@���@��P@�S�@�
=@�v�@�-@�J@�J@���@�hs@��@�Ĝ@���@��@�r�@�Z@�9X@�1@�l�@�ȴ@�ff@�5?@�@��@��-@��h@�O�@�G�@�7L@�&�@��@��/@���@�Q�@��@���@���@�\)@�"�@��@��@��@�~�@�E�@��@��@��@�1'@��m@���@�\)@���@�J@��/@�Z@�1@��;@��@���@�K�@�+@��@���@���@���@�V@�5?@��@��h@���@���@�bN@� �@�b@�  @��
@���@�"�@��\@�ff@��@�p�@��/@��j@��9@���@�r�@�A�@�(�@���@��m@��;@��@�dZ@�"�@���@�~�@�ff@�V@�-@��T@��-@���@���@�hs@�&�@��@�V@���@�1'@���@��H@��R@���@���@�v�@�M�@�J@���@���@�O�@��/@��@�9X@�b@���@�dZ@��@��#@�p�@�O�@�?}@��@��D@�b@��m@��w@�+@��R@�~�@�M�@�{@��-@�`B@�O�@�/@�V@�%@��`@��9@���@��u@�z�@�z�@�r�@�(�@�@�;@�P@~�R@~@}�@|9X@|�@{�
@{�@z^5@x��@xbN@x  @w��@v��@v��@v�+@vv�@u�h@t��@s�
@s��@st�@r�@r��@rn�@rJ@q�^@qhs@q&�@p��@p �@o��@o\)@n�@n@m`B@l�/@l�@k��@j�\@i��@i7L@h��@h��@h�@hA�@h1'@hb@g��@g\)@f�R@fv�@fE�@fE�@f@ep�@e/@eV@d�@d��@dZ@cƨ@c33@c@b�!@b~�@b�@ax�@a�@a%@`��@`Q�@_�@^��@^ff@]��@]?}@\��@\��@\�j@\j@\1@[�
@[��@Z�@Z�@Yx�@Yhs@Y%@X�@X �@W;d@V$�@V@V@U�@U�-@U/@U�@U�@T�@T�@Tz�@T9X@S�
@St�@S33@S@R��@RM�@R�@RJ@Q�@Q�^@QG�@Q&�@Q%@PQ�@O�@OK�@Nȴ@N��@NE�@M�T@M��@M�-@M�h@Mp�@M�@L��@L�@L�D@Lz�@LZ@L9X@K��@Kƨ@K��@KS�@KC�@K33@Ko@J^5@I��@IX@IX@IG�@I�@H��@H�u@HA�@Hb@H  @G�;@G�@G�P@G\)@F�@E�@EO�@D��@C��@C33@B��@B��@B�\@B~�@Bn�@B=q@Ahs@A7L@A&�@A&�@A�@A�@A�@@�@?�@?��@?�@?��@?K�@>��@>v�@=�@=�h@<�@<�@<z�@<(�@;�
@;��@;S�@:=q@9��@9�^@9�7@9X@97L@97L@8��@8A�@8  @7��@7�@7|�@7K�@6�y@6{@5��@5`B@5V@4��@4�@4j@41@3�
@3ƨ@3ƨ@3�F@3�@3dZ@3"�@2��@2~�@2n�@2^5@2�@1�@1��@1hs@1G�@0�9@0bN@/��@/K�@/;d@.��@.�R@.�+@.{@-�@-?}@,�@,�/@,��@,j@+�m@+��@+dZ@+dZ@+S�@+"�@*��@*�\@)�@)x�@)�@(�9@(A�@(  @'l�@'+@'
=@&�y@&�R@&��@&v�@&$�@&{@%�@%�@%��@%�-@%�h@%�h@%p�@%p�@%p�@%O�@$j@#��@"�@"��@"�!@"^5@"=q@"M�@"-@"J@!��@!x�@!hs@!G�@ ��@ �9@ r�@ 1'@�;@��@�P@;d@ȴ@v�@5?@{@�-@�@�@��@j@I�@I�@(�@1@��@�m@ƨ@�F@�F@C�@�H@��@~�@^5@�@�@�#@�#@��@��@�^@��@��@hs@X@7L@%@�9@r�@b@��@�w@��@l�@;d@ȴ@v�@$�@��@�@p�@p�@p�@p�@`B@`B@?}@/@/@�@�@V@�/@�@j@�m@��@33@33@�H@�H@��@��@��@~�@=q@�@J@�#@��@��@��@x�@X@7L@�@��@��@��@�@bN@ �@b@b@�@�P@K�@K�@+@�R@v�@ff@5?@$�@{@�T@@��@/@��@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�K�A�Q�A�S�A�^5A�`BA�`BA�`BA�dZA�ffA�ffA�\)A�XA�VA�ZA�\)A�ZA�\)A�O�A�C�A�O�A�O�A�K�A�C�A�&�A�1A�1A�1A�  A�  A���A���A��mA���A״9Aם�Aח�Aו�Aׇ+A�ffA�;dA�bAִ9A�t�A�ƨA�A��A�n�A���A�C�A�bA�ZA�"�AÅA�A¥�A�bA���A���A��PA���A�p�A��A��A���A�"�A�O�A�JA���A��#A�I�A��\A��hA���A�"�A�x�A�+A���A��A�  A���A�7LA��yA�$�A�O�A�E�A�p�A�VAy��Ay7LAx�Aw�PAvQ�AuXAp�Ae\)A`��AZ�AT^5ANz�AE7LAB�/A@z�A?A>�9A=%A;&�A9XA7�#A7�A6�A6=qA5VA4�A4Q�A3�A3A1t�A,�yA+�#A+�hA*�`A*�jA*�A*�+A*��A*v�A)+A'A'
=A&n�A%"�A$ĜA$��A$^5A$�A#��A"��A!S�A ��A��A�A�7A�RA9XA��A�A�A�PAK�AA��AVAE�A=qA1'AJA��A�DA��A��A�A$�A;dA��A�DAA�AA&�A�yAZA�hA/A%A��A�uA^5AffA{AƨAdZA�jA�+A{A�7A�AȴA��A�A?}AA
��A
��A
�A
��A
jA
1'A
{A	��A	l�A�DA9XA �AƨA&�A�+AQ�A1A��A��AdZAĜA��AbNA �A�A\)A��A�9A��A��A�\AjA  Al�AK�A/A �A ^5A 1'A �@��F@�+@���@�@�@�O�@�Ĝ@��@��P@�
=@�E�@���@���@��h@��@��@�j@�b@��w@��@���@�l�@�;d@���@��@��@��m@�"�@�J@�X@�Z@�^5@��-@�hs@��/@�r�@�bN@�w@�S�@���@�M�@陚@�/@�j@�(�@�ƨ@�C�@��H@�=q@�J@�-@��@�bN@�|�@�V@�h@��@�z�@��;@߅@�S�@�+@�n�@��`@ܣ�@܋D@�z�@�bN@�  @��y@ٲ-@�1'@�S�@���@֗�@�^5@��@թ�@�hs@Լj@�Q�@ӶF@ҏ\@��@�Ĝ@�j@�Q�@�(�@Ϯ@�dZ@�33@��@Χ�@���@�7L@���@�bN@��@���@���@ˮ@˅@�t�@�K�@�
=@ʸR@�~�@�V@�$�@��@�@�hs@�%@ț�@�1'@���@ǝ�@�33@ƸR@ř�@�?}@��`@ļj@ģ�@ēu@ċD@ă@ă@�z�@�r�@�1'@Å@�\)@�ȴ@��@��h@�G�@��@��D@���@�t�@��H@�^5@�^5@�-@��@�@��@�G�@�Ĝ@��w@�+@��H@��\@��@���@��w@��w@��w@��F@��P@�;d@���@���@�x�@�&�@�j@�b@���@��F@�l�@��@��\@�~�@�~�@�^5@�$�@��@���@���@��@�7L@���@���@��@�(�@��w@�K�@�@�~�@�{@�V@��9@�A�@�o@�n�@�=q@���@��D@���@��P@�S�@�
=@�v�@�-@�J@�J@���@�hs@��@�Ĝ@���@��@�r�@�Z@�9X@�1@�l�@�ȴ@�ff@�5?@�@��@��-@��h@�O�@�G�@�7L@�&�@��@��/@���@�Q�@��@���@���@�\)@�"�@��@��@��@�~�@�E�@��@��@��@�1'@��m@���@�\)@���@�J@��/@�Z@�1@��;@��@���@�K�@�+@��@���@���@���@�V@�5?@��@��h@���@���@�bN@� �@�b@�  @��
@���@�"�@��\@�ff@��@�p�@��/@��j@��9@���@�r�@�A�@�(�@���@��m@��;@��@�dZ@�"�@���@�~�@�ff@�V@�-@��T@��-@���@���@�hs@�&�@��@�V@���@�1'@���@��H@��R@���@���@�v�@�M�@�J@���@���@�O�@��/@��@�9X@�b@���@�dZ@��@��#@�p�@�O�@�?}@��@��D@�b@��m@��w@�+@��R@�~�@�M�@�{@��-@�`B@�O�@�/@�V@�%@��`@��9@���@��u@�z�@�z�@�r�@�(�@�@�;@�P@~�R@~@}�@|9X@|�@{�
@{�@z^5@x��@xbN@x  @w��@v��@v��@v�+@vv�@u�h@t��@s�
@s��@st�@r�@r��@rn�@rJ@q�^@qhs@q&�@p��@p �@o��@o\)@n�@n@m`B@l�/@l�@k��@j�\@i��@i7L@h��@h��@h�@hA�@h1'@hb@g��@g\)@f�R@fv�@fE�@fE�@f@ep�@e/@eV@d�@d��@dZ@cƨ@c33@c@b�!@b~�@b�@ax�@a�@a%@`��@`Q�@_�@^��@^ff@]��@]?}@\��@\��@\�j@\j@\1@[�
@[��@Z�@Z�@Yx�@Yhs@Y%@X�@X �@W;d@V$�@V@V@U�@U�-@U/@U�@U�@T�@T�@Tz�@T9X@S�
@St�@S33@S@R��@RM�@R�@RJ@Q�@Q�^@QG�@Q&�@Q%@PQ�@O�@OK�@Nȴ@N��@NE�@M�T@M��@M�-@M�h@Mp�@M�@L��@L�@L�D@Lz�@LZ@L9X@K��@Kƨ@K��@KS�@KC�@K33@Ko@J^5@I��@IX@IX@IG�@I�@H��@H�u@HA�@Hb@H  @G�;@G�@G�P@G\)@F�@E�@EO�@D��@C��@C33@B��@B��@B�\@B~�@Bn�@B=q@Ahs@A7L@A&�@A&�@A�@A�@A�@@�@?�@?��@?�@?��@?K�@>��@>v�@=�@=�h@<�@<�@<z�@<(�@;�
@;��@;S�@:=q@9��@9�^@9�7@9X@97L@97L@8��@8A�@8  @7��@7�@7|�@7K�@6�y@6{@5��@5`B@5V@4��@4�@4j@41@3�
@3ƨ@3ƨ@3�F@3�@3dZ@3"�@2��@2~�@2n�@2^5@2�@1�@1��@1hs@1G�@0�9@0bN@/��@/K�@/;d@.��@.�R@.�+@.{@-�@-?}@,�@,�/@,��@,j@+�m@+��@+dZ@+dZ@+S�@+"�@*��@*�\@)�@)x�@)�@(�9@(A�@(  @'l�@'+@'
=@&�y@&�R@&��@&v�@&$�@&{@%�@%�@%��@%�-@%�h@%�h@%p�@%p�@%p�@%O�@$j@#��@"�@"��@"�!@"^5@"=q@"M�@"-@"J@!��@!x�@!hs@!G�@ ��@ �9@ r�@ 1'@�;@��@�P@;d@ȴ@v�@5?@{@�-@�@�@��@j@I�@I�@(�@1@��@�m@ƨ@�F@�F@C�@�H@��@~�@^5@�@�@�#@�#@��@��@�^@��@��@hs@X@7L@%@�9@r�@b@��@�w@��@l�@;d@ȴ@v�@$�@��@�@p�@p�@p�@p�@`B@`B@?}@/@/@�@�@V@�/@�@j@�m@��@33@33@�H@�H@��@��@��@~�@=q@�@J@�#@��@��@��@x�@X@7L@�@��@��@��@�@bN@ �@b@b@�@�P@K�@K�@+@�R@v�@ff@5?@$�@{@�T@@��@/@��@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�oB
�oB
�oB
�oB
�uB
�uB
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�uB
t�B
9XB
I�B
�\B
��B
�?B
�wB
��B
��B
��B
�B
�5B
=B%�B8RBN�Bn�Bu�Bx�B~�B�B�JB��B�!B�LB�3B�B��B��Bz�BJ�B'�B
��B
�B
��B
�!B
�hB
w�B
^5B
H�B
49B
&�B
�B	�mB	�`B	�NB	�)B	��B	ɺB	�'B	z�B	]/B	C�B	,B	-B	(�B	0!B	<jB	<jB	?}B	G�B	T�B	_;B	u�B	�B	�B	�=B	��B	��B	��B	��B	�B	�'B	��B	�=B	��B	��B	��B	��B	�B	�9B	�^B	�?B	�3B	�B	�!B	�9B	�?B	�qB	ƨB	ȴB	ǮB	ȴB	ƨB	ĜB	B	��B	��B	ÖB	��B	�)B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
	7B
B
B
B
B
%B

=B
\B
hB
oB
oB
uB
oB
VB
VB
uB
{B
�B
�B
�B
"�B
!�B
!�B
�B
 �B
!�B
$�B
%�B
)�B
(�B
)�B
(�B
(�B
'�B
&�B
&�B
&�B
%�B
%�B
$�B
$�B
$�B
$�B
$�B
$�B
#�B
%�B
'�B
'�B
)�B
-B
.B
/B
2-B
0!B
0!B
/B
0!B
.B
-B
-B
.B
.B
.B
.B
-B
,B
,B
,B
,B
+B
+B
+B
+B
+B
)�B
(�B
(�B
(�B
'�B
'�B
&�B
%�B
%�B
$�B
$�B
$�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
"�B
"�B
"�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
{B
uB
uB
oB
oB
hB
hB
\B
\B
VB
VB
VB
VB
VB
VB
PB
PB
PB
PB
JB
JB
JB
JB
JB
DB

=B

=B

=B

=B

=B

=B

=B

=B

=B
	7B
	7B
	7B
1B
	7B
	7B
	7B
DB
DB
DB

=B
	7B
	7B

=B

=B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
1B
1B
	7B
	7B
	7B
1B
1B
1B
+B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
+B
+B
+B
+B
+B
	7B
	7B
1B
1B
1B
1B
	7B
1B
	7B
1B
+B
+B
+B
+B
	7B
1B
1B
1B
1B
1B
1B
1B

=B
	7B
	7B

=B

=B

=B

=B

=B
DB
DB
DB
DB
DB
DB
DB
DB
JB
JB
PB
PB
JB
PB
PB
PB
VB
VB
VB
VB
VB
VB
VB
bB
\B
\B
bB
hB
hB
hB
hB
oB
oB
uB
oB
oB
oB
uB
uB
uB
uB
{B
{B
uB
{B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
&�B
&�B
'�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
)�B
+B
,B
,B
,B
,B
-B
-B
-B
-B
.B
-B
.B
.B
.B
.B
.B
-B
-B
-B
+B
+B
+B
+B
,B
,B
,B
,B
-B
.B
.B
.B
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
33B
33B
49B
49B
49B
49B
49B
33B
33B
33B
33B
33B
33B
49B
5?B
5?B
5?B
6FB
7LB
8RB
8RB
7LB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
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
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
M�B
N�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
Q�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
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
W
B
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
XB
XB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
e`B
ffB
ffB
ffB
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
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
jB
jB
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
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
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
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
t�B
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
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
x�B
x�B
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
{�B
|�B
|�B
}�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�oB
�oB
�oB
�oB
�uB
�uB
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�uB
t�B
9XB
I�B
�\B
��B
�?B
�wB
��B
��B
��B
�B
�5B
=B%�B8RBN�Bn�Bu�Bx�B~�B�B�JB��B�!B�LB�3B�B��B��Bz�BJ�B'�B
��B
�B
��B
�!B
�hB
w�B
^5B
H�B
49B
&�B
�B	�mB	�`B	�NB	�)B	��B	ɺB	�'B	z�B	]/B	C�B	,B	-B	(�B	0!B	<jB	<jB	?}B	G�B	T�B	_;B	u�B	�B	�B	�=B	��B	��B	��B	��B	�B	�'B	��B	�=B	��B	��B	��B	��B	�B	�9B	�^B	�?B	�3B	�B	�!B	�9B	�?B	�qB	ƨB	ȴB	ǮB	ȴB	ƨB	ĜB	B	��B	��B	ÖB	��B	�)B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
	7B
B
B
B
B
%B

=B
\B
hB
oB
oB
uB
oB
VB
VB
uB
{B
�B
�B
�B
"�B
!�B
!�B
�B
 �B
!�B
$�B
%�B
)�B
(�B
)�B
(�B
(�B
'�B
&�B
&�B
&�B
%�B
%�B
$�B
$�B
$�B
$�B
$�B
$�B
#�B
%�B
'�B
'�B
)�B
-B
.B
/B
2-B
0!B
0!B
/B
0!B
.B
-B
-B
.B
.B
.B
.B
-B
,B
,B
,B
,B
+B
+B
+B
+B
+B
)�B
(�B
(�B
(�B
'�B
'�B
&�B
%�B
%�B
$�B
$�B
$�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
"�B
"�B
"�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
{B
uB
uB
oB
oB
hB
hB
\B
\B
VB
VB
VB
VB
VB
VB
PB
PB
PB
PB
JB
JB
JB
JB
JB
DB

=B

=B

=B

=B

=B

=B

=B

=B

=B
	7B
	7B
	7B
1B
	7B
	7B
	7B
DB
DB
DB

=B
	7B
	7B

=B

=B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
1B
1B
	7B
	7B
	7B
1B
1B
1B
+B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
+B
+B
+B
+B
+B
	7B
	7B
1B
1B
1B
1B
	7B
1B
	7B
1B
+B
+B
+B
+B
	7B
1B
1B
1B
1B
1B
1B
1B

=B
	7B
	7B

=B

=B

=B

=B

=B
DB
DB
DB
DB
DB
DB
DB
DB
JB
JB
PB
PB
JB
PB
PB
PB
VB
VB
VB
VB
VB
VB
VB
bB
\B
\B
bB
hB
hB
hB
hB
oB
oB
uB
oB
oB
oB
uB
uB
uB
uB
{B
{B
uB
{B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
&�B
&�B
'�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
)�B
+B
,B
,B
,B
,B
-B
-B
-B
-B
.B
-B
.B
.B
.B
.B
.B
-B
-B
-B
+B
+B
+B
+B
,B
,B
,B
,B
-B
.B
.B
.B
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
33B
33B
49B
49B
49B
49B
49B
33B
33B
33B
33B
33B
33B
49B
5?B
5?B
5?B
6FB
7LB
8RB
8RB
7LB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
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
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
M�B
N�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
Q�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
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
W
B
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
XB
XB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
e`B
ffB
ffB
ffB
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
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
jB
jB
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
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
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
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
t�B
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
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
x�B
x�B
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
{�B
|�B
|�B
}�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.09 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210720100106                              AO  ARCAADJP                                                                    20210720100106    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210720100106  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210720100106  QCF$                G�O�G�O�G�O�0               