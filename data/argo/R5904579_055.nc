CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS      N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T13:44:09Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  N   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ^�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  b�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  s0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �P   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �p   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �\   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �l   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �p   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024134409  20181024134409  5904579 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               7A   AO  5429                            2B  A   NAVIS_A                         0450                            011514                          863 @���݄1   @���hV@&t�j~���c��n��1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      7A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�33B�33B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3E   E   E @ E ` E � E � E � E � E  E  E@ E` E� E� E� E� E  E  E@ E` E� E� E� E� E  E  E@ E` E� E� E� E� E  Ef111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��
@�p�A�RA"�RAB�RAb�RA�\)A�\)A�\)A��\A�\)A�\)A�\)A�\)B �B�B�B�B �B(�B1zB8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�W
B�W
B�W
B�W
B�W
B�#�B�W
B�W
B��=B�#�B�W
B�W
B�W
B�W
B�W
B�W
B��=BĊ=B�#�B�#�B�#�B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
C +�C+�C+�C+�C+�C
+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C +�C"+�C$+�C&+�C(+�C*+�C,+�C.+�C0+�C2+�C4+�C6+�C8+�C:+�C<+�C>+�C@+�CB+�CD+�CF+�CH+�CJ+�CL+�CN+�CP+�CR+�CT+�CV+�CX+�CZ+�C\+�C^+�C`+�Cb+�Cd+�Cf+�Ch+�Cj+�Cl+�Cn+�Cp+�Cr+�Ct+�Cv+�Cx+�Cz+�C|+�C~+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D 
�D ��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D	
�D	��D

�D
��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D 
�D ��D!
�D!��D"
�D"��D#
�D#��D$
�D$��D%
�D%��D&
�D&��D'
�D'��D(
�D(��D)
�D)��D*
�D*��D+
�D+��D,
�D,��D-
�D-��D.
�D.��D/
�D/��D0
�D0��D1
�D1��D2
�D2��D3
�D3��D4
�D4��D5
�D5��D6
�D6��D7
�D7��D8
�D8��D9
�D9��D:
�D:��D;
�D;��D<
�D<��D=
�D=��D>
�D>��D?
�D?��D@
�D@��DA
�DA��DB
�DB��DC
�DC��DD
�DD��DE
�DE��DF
�DF��DG
�DG��DH
�DH��DI
�DI��DJ
�DJ��DK
�DK��DL
�DL��DM
�DM��DN
�DN��DO
�DO��DP
�DP��DQ
�DQ��DR
�DR��DS
�DS��DT
�DT��DU
�DU��DV
�DV��DW
�DW��DX
�DX��DY
�DY��DZ
�DZ��D[
�D[��D\
�D\��D]
�D]��D^
�D^��D_
�D_��D`
�D`��Da
�Da��Db
�Db��Dc
�Dc��Dd
�Dd��De
�De��Df
�Df��Dg
�Dg��Dh
�Dh��Di
�Di��Dj
�Dj��Dk
�Dk��Dl
�Dl��Dm
�Dm��Dn
�Dn��Do
�Do��Dp
�Dp��Dq
�Dq��Dr
�Dr��Ds
�Ds��Dt
�Dt��Du
�Du��Dv
�Dv��Dw
�Dw��Dx
�Dx��Dy
�Dy��Dz
�Dz��D{
�D{��D|
�D|��D}
�D}��D~
�D~��D
�D��D�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD��D�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD�ȤD�qD�H�D��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�B>D��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqDqD��qD�qD�EqDÅqD��qD�qD�EqDąqD��qD�qD�EqDŅqD��qD�qD�EqDƅqD��qD�qD�EqDǅqD��qD�qD�EqDȅqD��qD�qD�EqDɅqD��qD�qD�EqDʅqD��qD�qD�EqD˅qD��qD�qD�EqD̅qD��qD�qD�EqDͅqD��qD�qD�EqD΅qD��qD�qD�EqDυqD��qD�qD�EqDЅqD��qD�qD�EqDхqD��qD�qD�EqD҅qD��qD�qD�EqDӅqD��qD�qD�EqDԅqD��qD�qD�EqDՅqD��qD�qD�EqDօqD��qD�qD�EqDׅqD��qD�qD�EqD؅qD��qD�qD�EqDمqD��qD�qD�EqDڅqD��qD�qD�EqDۅqD��qD�qD�EqD܅qD��qD�qD�EqD݅qD��qD�qD�EqDޅqD��qD�qD�EqD߅qD��qD�qD�EqD��qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD��qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD�ȤE �E "�E B�E b�E ��E ��E ¸E �E�E"�EB�Eb�E��E��E¸E�E�E"�EB�Eb�E��E��E¸E�E�E"�EB�Eb�E��E��E¸E�E�E111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�JA�E�A�l�A�$�A�+A�A���A��
A��HA��A�7A�A��yA�Q�A���A�
=A���A�+A��A��`A�ZA�`BA���A�?}A۰!A��A��A�r�AֶFAѾwA�r�A�ȴA���A��#A���Að!A�n�A�n�A��A�JA���A��A��A�C�A�hsA�
=A��A��A�VA��A�l�Ay��Av�Aq�#Am
=Aj  Ag/AbJA]K�AY�AV1AP��AK+AH��AG��AF�!AE|�AC�7AB~�ABQ�AA��AAx�A?�#A>��A>A=��A=�A=�A=VA<�RA<�9A<�A<�DA;&�A9%A8JA8�A8�A7��A7l�A7A6�+A5��A5|�A5&�A4�DA3�A3XA3O�A3�A2z�A2-A21A1��A1+A0��A0r�A0Q�A0A�A/�A/�A/\)A/G�A/�A/VA.��A.��A.�\A.bNA.5?A-�TA-��A-t�A-\)A-+A-%A,�A,��A,E�A,�A+�TA+�^A+G�A*��A*ĜA*��A*��A*A)�A)+A(��A(�RA(�A(ZA(�A'��A'�;A'�FA'|�A'`BA'33A';dA&��A& �A%�A%S�A$�yA$�RA$��A$v�A$9XA$1A#�hA#O�A#C�A#oA"�`A"��A"M�A!��A!7LA ��A jA E�A��Ax�AC�A��A��A �A��AhsAS�A;dA�A��A�RAz�A�^A33A�jA=qA{A�#AXA�!A^5A=qA$�A�A��A��A�7At�AoA�An�AVA=qAbAƨA33Ar�AE�A�A�^A��AhsA�9A��A�DAZA  Ap�A%A�A��AbNA��AC�AVA�+AJA��AC�A�/Av�AVA-A��A�PAp�AG�A33A
�`A
(�A	�-A	|�A	/A	A	�A��A^5A$�A��A��AbNA��A;dA��A��A�A��AC�A�/A�An�A��A�A7LA r�A J@�o@���@�M�@���@��@�M�@���@��;@�o@�ff@��@�G�@��`@�j@�@�p�@�/@���@�A�@���@��@��y@�h@�r�@�t�@�~�@�-@��@�l�@�@�@�&�@��`@�S�@⟾@��@�p�@��@�I�@��@�{@ܣ�@۶F@�dZ@��@���@�ff@ٙ�@���@�A�@֟�@��@�`B@ԓu@��@���@ӍP@ҸR@���@ѡ�@�hs@���@д9@�9X@�\)@θR@�=q@��#@Ͳ-@���@���@�dZ@��@�n�@�@��@��@ǅ@�"�@�ȴ@�ff@��#@�`B@�%@�(�@�dZ@��@�v�@��@��-@�(�@�\)@�+@���@���@�$�@���@��@�7L@��@�(�@��@�ȴ@�{@��h@�/@���@�bN@��@�  @��m@��w@��@�@�5?@��#@��@��@���@��D@�Q�@�9X@��@��m@���@�\)@��y@�v�@��@���@��7@��j@�b@���@�33@�@��@���@���@�n�@�5?@��#@�O�@���@��/@��/@���@�A�@� �@�  @�ƨ@���@�|�@�K�@�"�@�
=@���@��@��#@�O�@��j@��m@���@�;d@��@�~�@�V@�E�@�-@�@���@���@�/@��j@��u@�9X@��@�|�@�33@�o@���@��+@�ff@�J@�7L@���@�Q�@��;@��F@���@�v�@��@���@��@��u@���@�\)@���@�^5@�p�@�?}@���@� �@��;@���@��@��@�;d@�^5@��^@�p�@���@�r�@�9X@�  @��@�;d@��@�
=@��@���@�ff@��#@��h@�G�@��@��u@�Q�@��@���@��m@�dZ@���@�ȴ@���@�^5@�-@��@��^@��@��@��`@��u@�Z@�1'@��@�ƨ@���@���@���@�dZ@�"�@��H@��\@�v�@�ff@�$�@���@��7@��@�x�@�p�@�X@�?}@��/@�bN@���@�dZ@�S�@�+@��y@��R@��+@�n�@�$�@��@���@��@�G�@��`@��u@�j@��@�  @���@��@�\)@�33@���@�V@�=q@���@�@���@���@��7@�X@���@���@��@�@~�+@~{@}��@}�@|�/@|��@|9X@{��@{"�@z��@z-@y�^@yX@xr�@w��@w\)@v��@v�R@v�+@u�T@u�@uO�@t��@t�D@t�@s�
@st�@r��@q��@qX@q%@pr�@pbN@pQ�@p1'@p �@pb@pb@o�@o�@o+@nv�@nE�@m��@m�@l��@l�@k�m@k��@kS�@k"�@j��@j�\@jM�@jJ@i�#@iG�@h�9@hQ�@h  @g�@g\)@g+@f��@f�+@f5?@e��@e�@e?}@eV@d�@dz�@dZ@d(�@c�F@c�@cdZ@co@bM�@a��@ax�@a%@`�`@`Ĝ@`�u@`Q�@`A�@`1'@` �@`  @_
=@^ff@^$�@]�@]@]p�@\��@\j@\9X@\(�@\(�@[�
@["�@Z��@Z-@Yx�@YX@YG�@Y&�@Y%@X�9@X �@W�w@W|�@W+@V��@V�+@V5?@V$�@U�-@U�@T�/@Tj@S��@St�@S"�@R�H@R��@R=q@Q��@Qhs@Q%@P�9@PA�@P �@P  @O�@Ol�@O�@N�y@Nȴ@N�+@NE�@M�-@L�@L��@L��@L9X@Kƨ@KC�@J�@J�!@Jn�@J�@I�#@I��@IX@I%@H��@H�9@H�u@H�@HA�@G�@G�P@F�y@F�R@F�R@F��@FE�@F$�@E�@EO�@DI�@Cƨ@C��@CdZ@C33@B��@B^5@Ahs@A&�@@��@@�u@@1'@?��@?K�@?�@>�R@>v�@>5?@>@=��@=��@=`B@=?}@=�@<��@<��@<z�@;�m@;33@:�H@:�H@:��@:�\@:-@9��@9x�@9G�@9&�@8��@7�@7|�@7;d@6�y@6��@65?@5�T@5�@5/@4�@4��@4Z@3��@3��@3C�@3o@2��@2��@2�@1�7@1�@0��@0�@0  @/��@/�@/|�@/\)@/�@.ȴ@.��@.ff@.E�@.$�@-�@-`B@,��@,z�@,I�@+��@+�@+C�@+@*�H@*��@*��@*^5@)�@)��@)�7@)G�@)�@(��@(�u@( �@(b@'�@'��@'�@'�P@'
=@&�@&v�@%�@%�-@%�h@%`B@%O�@%/@%�@%V@$�@$�j@$z�@$Z@#��@#t�@#33@#"�@#"�@#o@"�H@"�\@"n�@"M�@!�@!��@!x�@!&�@ ��@ ��@ Q�@ 1'@   @�w@�@|�@K�@+@�@�y@��@�+@v�@ff@ff@5?@@��@p�@`B@��@�@9X@�F@�@t�@S�@o@�H@��@��@~�@�@J@�^@�7@G�@��@�@A�@1'@ �@  @�@�;@�@l�@\)@;d@
=@��@E�@@O�@�@�@�j@j@9X@1@�
@ƨ@ƨ@ƨ@�F@�F@��@�@dZ@"�@�@��@��@�!@��@��@n�@^5@=q@�@��@��@��@�@�#@�7@�@��@Ĝ@Ĝ@Ĝ@Ĝ@�u@1'@ �@ �@ �@�;@�P@K�@+@��@�y@��@v�@E�@�T@�h@?}@��@��@�j@�@�@�@�@��@�D@z�@Z@(�@�
@�@C�@o@@
�\@
^5@
M�@
=q@
-@
-@
�@
J@	��@	hs@	7L@	7L@	�@��@��@�`@Ĝ@�9@�u@r�@Q�@1'@b@�@�w@�P@\)@;d@�@�y@ȴ@��@ff@V@5?@{@�@��@�-@�h@`B@/@V@��@�@�/@��@�j@��@z�@j@Z@I�@9X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�JA�E�A�l�A�$�A�+A�A���A��
A��HA��A�7A�A��yA�Q�A���A�
=A���A�+A��A��`A�ZA�`BA���A�?}A۰!A��A��A�r�AֶFAѾwA�r�A�ȴA���A��#A���Að!A�n�A�n�A��A�JA���A��A��A�C�A�hsA�
=A��A��A�VA��A�l�Ay��Av�Aq�#Am
=Aj  Ag/AbJA]K�AY�AV1AP��AK+AH��AG��AF�!AE|�AC�7AB~�ABQ�AA��AAx�A?�#A>��A>A=��A=�A=�A=VA<�RA<�9A<�A<�DA;&�A9%A8JA8�A8�A7��A7l�A7A6�+A5��A5|�A5&�A4�DA3�A3XA3O�A3�A2z�A2-A21A1��A1+A0��A0r�A0Q�A0A�A/�A/�A/\)A/G�A/�A/VA.��A.��A.�\A.bNA.5?A-�TA-��A-t�A-\)A-+A-%A,�A,��A,E�A,�A+�TA+�^A+G�A*��A*ĜA*��A*��A*A)�A)+A(��A(�RA(�A(ZA(�A'��A'�;A'�FA'|�A'`BA'33A';dA&��A& �A%�A%S�A$�yA$�RA$��A$v�A$9XA$1A#�hA#O�A#C�A#oA"�`A"��A"M�A!��A!7LA ��A jA E�A��Ax�AC�A��A��A �A��AhsAS�A;dA�A��A�RAz�A�^A33A�jA=qA{A�#AXA�!A^5A=qA$�A�A��A��A�7At�AoA�An�AVA=qAbAƨA33Ar�AE�A�A�^A��AhsA�9A��A�DAZA  Ap�A%A�A��AbNA��AC�AVA�+AJA��AC�A�/Av�AVA-A��A�PAp�AG�A33A
�`A
(�A	�-A	|�A	/A	A	�A��A^5A$�A��A��AbNA��A;dA��A��A�A��AC�A�/A�An�A��A�A7LA r�A J@�o@���@�M�@���@��@�M�@���@��;@�o@�ff@��@�G�@��`@�j@�@�p�@�/@���@�A�@���@��@��y@�h@�r�@�t�@�~�@�-@��@�l�@�@�@�&�@��`@�S�@⟾@��@�p�@��@�I�@��@�{@ܣ�@۶F@�dZ@��@���@�ff@ٙ�@���@�A�@֟�@��@�`B@ԓu@��@���@ӍP@ҸR@���@ѡ�@�hs@���@д9@�9X@�\)@θR@�=q@��#@Ͳ-@���@���@�dZ@��@�n�@�@��@��@ǅ@�"�@�ȴ@�ff@��#@�`B@�%@�(�@�dZ@��@�v�@��@��-@�(�@�\)@�+@���@���@�$�@���@��@�7L@��@�(�@��@�ȴ@�{@��h@�/@���@�bN@��@�  @��m@��w@��@�@�5?@��#@��@��@���@��D@�Q�@�9X@��@��m@���@�\)@��y@�v�@��@���@��7@��j@�b@���@�33@�@��@���@���@�n�@�5?@��#@�O�@���@��/@��/@���@�A�@� �@�  @�ƨ@���@�|�@�K�@�"�@�
=@���@��@��#@�O�@��j@��m@���@�;d@��@�~�@�V@�E�@�-@�@���@���@�/@��j@��u@�9X@��@�|�@�33@�o@���@��+@�ff@�J@�7L@���@�Q�@��;@��F@���@�v�@��@���@��@��u@���@�\)@���@�^5@�p�@�?}@���@� �@��;@���@��@��@�;d@�^5@��^@�p�@���@�r�@�9X@�  @��@�;d@��@�
=@��@���@�ff@��#@��h@�G�@��@��u@�Q�@��@���@��m@�dZ@���@�ȴ@���@�^5@�-@��@��^@��@��@��`@��u@�Z@�1'@��@�ƨ@���@���@���@�dZ@�"�@��H@��\@�v�@�ff@�$�@���@��7@��@�x�@�p�@�X@�?}@��/@�bN@���@�dZ@�S�@�+@��y@��R@��+@�n�@�$�@��@���@��@�G�@��`@��u@�j@��@�  @���@��@�\)@�33@���@�V@�=q@���@�@���@���@��7@�X@���@���@��@�@~�+@~{@}��@}�@|�/@|��@|9X@{��@{"�@z��@z-@y�^@yX@xr�@w��@w\)@v��@v�R@v�+@u�T@u�@uO�@t��@t�D@t�@s�
@st�@r��@q��@qX@q%@pr�@pbN@pQ�@p1'@p �@pb@pb@o�@o�@o+@nv�@nE�@m��@m�@l��@l�@k�m@k��@kS�@k"�@j��@j�\@jM�@jJ@i�#@iG�@h�9@hQ�@h  @g�@g\)@g+@f��@f�+@f5?@e��@e�@e?}@eV@d�@dz�@dZ@d(�@c�F@c�@cdZ@co@bM�@a��@ax�@a%@`�`@`Ĝ@`�u@`Q�@`A�@`1'@` �@`  @_
=@^ff@^$�@]�@]@]p�@\��@\j@\9X@\(�@\(�@[�
@["�@Z��@Z-@Yx�@YX@YG�@Y&�@Y%@X�9@X �@W�w@W|�@W+@V��@V�+@V5?@V$�@U�-@U�@T�/@Tj@S��@St�@S"�@R�H@R��@R=q@Q��@Qhs@Q%@P�9@PA�@P �@P  @O�@Ol�@O�@N�y@Nȴ@N�+@NE�@M�-@L�@L��@L��@L9X@Kƨ@KC�@J�@J�!@Jn�@J�@I�#@I��@IX@I%@H��@H�9@H�u@H�@HA�@G�@G�P@F�y@F�R@F�R@F��@FE�@F$�@E�@EO�@DI�@Cƨ@C��@CdZ@C33@B��@B^5@Ahs@A&�@@��@@�u@@1'@?��@?K�@?�@>�R@>v�@>5?@>@=��@=��@=`B@=?}@=�@<��@<��@<z�@;�m@;33@:�H@:�H@:��@:�\@:-@9��@9x�@9G�@9&�@8��@7�@7|�@7;d@6�y@6��@65?@5�T@5�@5/@4�@4��@4Z@3��@3��@3C�@3o@2��@2��@2�@1�7@1�@0��@0�@0  @/��@/�@/|�@/\)@/�@.ȴ@.��@.ff@.E�@.$�@-�@-`B@,��@,z�@,I�@+��@+�@+C�@+@*�H@*��@*��@*^5@)�@)��@)�7@)G�@)�@(��@(�u@( �@(b@'�@'��@'�@'�P@'
=@&�@&v�@%�@%�-@%�h@%`B@%O�@%/@%�@%V@$�@$�j@$z�@$Z@#��@#t�@#33@#"�@#"�@#o@"�H@"�\@"n�@"M�@!�@!��@!x�@!&�@ ��@ ��@ Q�@ 1'@   @�w@�@|�@K�@+@�@�y@��@�+@v�@ff@ff@5?@@��@p�@`B@��@�@9X@�F@�@t�@S�@o@�H@��@��@~�@�@J@�^@�7@G�@��@�@A�@1'@ �@  @�@�;@�@l�@\)@;d@
=@��@E�@@O�@�@�@�j@j@9X@1@�
@ƨ@ƨ@ƨ@�F@�F@��@�@dZ@"�@�@��@��@�!@��@��@n�@^5@=q@�@��@��@��@�@�#@�7@�@��@Ĝ@Ĝ@Ĝ@Ĝ@�u@1'@ �@ �@ �@�;@�P@K�@+@��@�y@��@v�@E�@�T@�h@?}@��@��@�j@�@�@�@�@��@�D@z�@Z@(�@�
@�@C�@o@@
�\@
^5@
M�@
=q@
-@
-@
�@
J@	��@	hs@	7L@	7L@	�@��@��@�`@Ĝ@�9@�u@r�@Q�@1'@b@�@�w@�P@\)@;d@�@�y@ȴ@��@ff@V@5?@{@�@��@�-@�h@`B@/@V@��@�@�/@��@�j@��@z�@j@Z@I�@9X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B�B1'B��B<jBVBdZBiyB�7Bz�B=qB�jB�mB	uB	33B	G�B	ffB	~�B	�=B	�{B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�uB	�uB	��B	��B	��B	��B	��B	��B	�3B	ÖB	�B	��B	�B	�B	�BB	��B
B
B	��B	�;B	��B	�}B	��B	��B	��B	��B	��B	�oB	��B	��B	��B	�!B	�`B
;dB
bNB
k�B
x�B
�B
�?B
�^B
�XB
B
��B
�TB
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��BBBDBJBJB
=BPBoBhBVBDBJBVBJBJB\BbBoBhBbBbBbB\B\B\B\BVBVBVB\B\BVBPBPBPBDBDBDB
=B	7B1B1B1B1B1B%BBB  BBB
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
�yB
�fB
�`B
�`B
�ZB
�NB
�NB
�HB
�;B
�5B
�/B
�#B
�#B
�B
�B
�B
�B
�
B
��B
��B
��B
��B
��B
��B
��B
ȴB
ǮB
ƨB
ǮB
ƨB
ƨB
ŢB
ŢB
ĜB
ÖB
��B
��B
��B
��B
��B
�}B
�wB
�dB
�XB
�XB
�LB
�FB
�?B
�'B
�'B
�'B
�!B
�B
�B
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
�{B
�{B
�uB
�oB
�hB
�bB
�bB
�bB
�bB
�JB
�=B
�7B
�7B
�1B
�JB
�JB
�DB
�7B
�1B
�B
�B
�B
� B
}�B
|�B
{�B
y�B
x�B
v�B
u�B
t�B
s�B
r�B
r�B
p�B
n�B
l�B
k�B
jB
hsB
gmB
e`B
cTB
aHB
_;B
`BB
_;B
_;B
_;B
^5B
]/B
\)B
[#B
ZB
ZB
YB
XB
W
B
T�B
R�B
P�B
O�B
O�B
N�B
L�B
K�B
K�B
I�B
H�B
H�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
H�B
I�B
I�B
I�B
I�B
H�B
H�B
G�B
F�B
E�B
E�B
E�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
C�B
D�B
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
C�B
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
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
D�B
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
G�B
H�B
H�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
L�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
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
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
Q�B
Q�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
T�B
T�B
T�B
T�B
T�B
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
XB
YB
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
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
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
`BB
`BB
`BB
aHB
aHB
aHB
`BB
aHB
aHB
bNB
cTB
bNB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
ffB
ffB
e`B
ffB
ffB
gmB
gmB
gmB
hsB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
jB
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
p�B
p�B
p�B
p�B
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
r�B
r�B
r�B
s�B
s�B
s�B
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
x�B
y�B
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
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�VB
�VB
�PB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
�{B
�{B
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
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�!B
�'B
�'B
�'B
�'B
�'B
�-B
�-B
�-B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�9B
�9B
�9B
�9B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�FB
�FB
�FB
�FB
�FB
�FB
�FB
�FB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�RB
�RB
�RB
�RB
�RB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�dB
�dB
�dB
�dB
�dB
�d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B�B1'B��B<jBVBdZBiyB�7Bz�B=qB�jB�mB	uB	33B	G�B	ffB	~�B	�=B	�{B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�uB	�uB	��B	��B	��B	��B	��B	��B	�3B	ÖB	�B	��B	�B	�B	�BB	��B
B
B	��B	�;B	��B	�}B	��B	��B	��B	��B	��B	�oB	��B	��B	��B	�!B	�`B
;dB
bNB
k�B
x�B
�B
�?B
�^B
�XB
B
��B
�TB
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��BBBDBJBJB
=BPBoBhBVBDBJBVBJBJB\BbBoBhBbBbBbB\B\B\B\BVBVBVB\B\BVBPBPBPBDBDBDB
=B	7B1B1B1B1B1B%BBB  BBB
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
�yB
�fB
�`B
�`B
�ZB
�NB
�NB
�HB
�;B
�5B
�/B
�#B
�#B
�B
�B
�B
�B
�
B
��B
��B
��B
��B
��B
��B
��B
ȴB
ǮB
ƨB
ǮB
ƨB
ƨB
ŢB
ŢB
ĜB
ÖB
��B
��B
��B
��B
��B
�}B
�wB
�dB
�XB
�XB
�LB
�FB
�?B
�'B
�'B
�'B
�!B
�B
�B
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
�{B
�{B
�uB
�oB
�hB
�bB
�bB
�bB
�bB
�JB
�=B
�7B
�7B
�1B
�JB
�JB
�DB
�7B
�1B
�B
�B
�B
� B
}�B
|�B
{�B
y�B
x�B
v�B
u�B
t�B
s�B
r�B
r�B
p�B
n�B
l�B
k�B
jB
hsB
gmB
e`B
cTB
aHB
_;B
`BB
_;B
_;B
_;B
^5B
]/B
\)B
[#B
ZB
ZB
YB
XB
W
B
T�B
R�B
P�B
O�B
O�B
N�B
L�B
K�B
K�B
I�B
H�B
H�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
H�B
I�B
I�B
I�B
I�B
H�B
H�B
G�B
F�B
E�B
E�B
E�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
C�B
D�B
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
C�B
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
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
D�B
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
G�B
H�B
H�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
L�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
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
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
Q�B
Q�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
T�B
T�B
T�B
T�B
T�B
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
XB
YB
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
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
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
`BB
`BB
`BB
aHB
aHB
aHB
`BB
aHB
aHB
bNB
cTB
bNB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
ffB
ffB
e`B
ffB
ffB
gmB
gmB
gmB
hsB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
jB
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
p�B
p�B
p�B
p�B
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
r�B
r�B
r�B
s�B
s�B
s�B
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
x�B
y�B
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
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�VB
�VB
�PB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
�{B
�{B
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
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�!B
�'B
�'B
�'B
�'B
�'B
�-B
�-B
�-B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�9B
�9B
�9B
�9B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�FB
�FB
�FB
�FB
�FB
�FB
�FB
�FB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�RB
�RB
�RB
�RB
�RB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�dB
�dB
�dB
�dB
�dB
�d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.17 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024134409                              AO  ARCAADJP                                                                    20181024134409    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024134409  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024134409  QCF$                G�O�G�O�G�O�0               