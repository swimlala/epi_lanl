CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-05-16T17:22:42Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         C   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    9   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    9    HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    9$   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    9(   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    98   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    9H   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    9X   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  9`   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  9�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  @  9�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        :    	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    :$   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    :(   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     :,   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    :L   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    :P   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     :T   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     :t   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     :�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    :�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ;�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_NB_SAMPLE_CTD_QC               	long_name         ,Global quality flag of NB_SAMPLE_CTD profile   conventions       Argo reference table 2a    
_FillValue                    ;�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  O�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  _0   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  r�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �d   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �P   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ̸   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Ф   NB_SAMPLE_CTD            
         	long_name         2Number of samples in each pressure bin for the CTD     
_FillValue        �     units         count      C_format      %5d    FORTRAN_format        I5     
resolution                �  �H   NB_SAMPLE_CTD_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  @  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �H   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �H   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �H   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  8  �H   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20230516172242  20230516172242  5906803 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            NB_SAMPLE_CTD      A   AO  9276                            2B  A   NAVIS_A                         1437                            170425                          863 @��?��>1   @�����@:�.H��d�)^�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         A   A   A       @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffBa33Bg��Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DL��DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D��D���D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�L�D�c311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�p�@�p�A�RA"�RAB�RAb�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B �B�B�B�B �B(�B0�B8�B@�BH�BP�BYzBa�GBhG�BpG�Bx�B�W
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
B�W
B�W
B܊=B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
C +�C+�C+�C+�C+�C
+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C +�C"+�C$+�C&+�C(+�C*+�C,+�C.+�C0+�C2+�C4+�C6+�C8+�C:+�C<+�C>+�C@+�CB+�CD+�CF+�CH+�CJ+�CL+�CN+�CP+�CR+�CT+�CV+�CX+�CZ+�C\+�C^+�C`+�Cb+�Cd+�Cf+�Ch+�Cj+�Cl+�Cn+�Cp+�Cr+�Ct+�Cv+�Cx+�Cz+�C|+�C~+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D 
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
�D�GD
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
�DL��DM{DM��DN
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
�D��D�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��>D��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqDqD��qD�qD�EqDÅqD��qD�qD�EqDąqD��qD�qD�EqDŅqD��qD�qD�EqDƅqD��qD�qD�EqDǅqD��qD�qD�EqDȅqD��qD�qD�EqDɅqD��qD�qD�EqDʅqD��qD�qD�EqD˅qD��qD�qD�EqD̅qD��qD�qD�EqDͅqD��qD�qD�EqD΅qD��qD�qD�EqDυqD��qD�qD�EqDЅqD��qD�qD�EqDхqD��qD�qD�EqD҅qD��qD�qD�EqDӅqD��qD�qD�EqDԅqD��qD�qD�EqDՅqD��qD�qD�EqDօqD��qD�qD�EqDׅqD��qD�qD�EqD؅qD��qD�qD�EqDمqD��qD�qD�EqDڅqD��qD�qD�EqDۅqD��qD�qD�EqD܅qD��qD�qD�EqD݅qD��qD�qD�EqDޅqD��qD�qD�EqD߅qD��qD�qD�EqD��qD��qD�qD�EqD�qD��qD�qD�EqD�qD��>D�>D�EqD�qD��qD�qD�H�D�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD��qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD�qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�EqD��qD��qD�qD�R>D�h�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A�ffA���A��A�{A��
Aա�A�~�A�ffA�K�A�$�A��A�ȴAԮAԣ�Aԗ�Aԇ+A�v�A�hsA�\)A�A�A�9XA�1'A� �A���Aә�A�~�A�dZA�M�A��A�-A�A�A�(�A�ĜA��mA�ĜA��HA�^5A��A��A���A��A��jA�~�A���A��A���A�Q�A�r�A�XA�A��A�A�A��A���A��A�ffA�ĜA���A�ȴA��A���A���A���A���A�5?A���A���A��FA�"�A�`BA�?}A���A�+A���A�A�z�A�%A��PA�oA��RA�`BA��;A�VA�x�A���A�z�A��`A���A�n�A���A� �A��A�1'A�z�A�33A��9A�E�A��A�(�A�v�A��A}�
A}?}Ax �Av�Au
=As|�Ao�#AnA�Al��AkƨAj�Ah�jAg�;AgdZAg;dAf��Ad�Ac��AcoA`�A^�DA[�FAZ�DAZ�AYƨAYO�AXI�AW|�AV�HAV��AVZAU��AS��AR�AO�AOp�AO;dAO33AO+AO"�AOVAN��AN��ANv�AM��AK�AG�#AE�^AEC�AE%AD1AC`BAB�`AB�AA�7A@�!A?��A=��A<��A:�A9��A9t�A9&�A8��A8��A8�9A8��A8Q�A7�mA7VA6JA5t�A4��A3hsA2��A2r�A1�wA1?}A0ffA/�7A.Q�A-&�A,�HA,5?A*jA)`BA(�jA'�A&  A%hsA%
=A$��A$^5A#��A#|�A"�\A ��AȴA?}A�;A33A��A$�AXA  A�yA��A��A��A�HA��A��A^5A5?A�AVA�uA=qAA\)AƨA%A
�yA
ĜA
��A	��A	dZA	oA�jA�AVA�AhsA�jA��At�AoA��AjAXA �/@�"�@��D@��7@�1'@�x�@�j@�v�@�^5@�=q@��-@�`B@��@���@� �@�o@�7L@���@��@��@���@�;d@ᙚ@��u@�l�@�p�@��T@�9X@ׅ@�S�@�=q@Ձ@���@��;@�\)@��@љ�@�`B@��@�A�@��H@�@�%@�S�@ə�@���@�A�@ǍP@�-@Ý�@��@��@�t�@�\)@�C�@�"�@���@���@�~�@��T@�I�@�ff@�@��-@��@�Z@���@�S�@�+@��R@�A�@��y@���@��@��j@�Z@��
@��@�o@���@�{@���@��7@�hs@�&�@��/@�r�@�A�@��@���@���@��P@�t�@�C�@��@��\@��#@���@�A�@���@��R@�E�@���@�hs@�Ĝ@�bN@��@�+@���@��+@�5?@��^@�bN@��@�l�@��@�~�@��@�?}@��9@�z�@�I�@�  @��
@��F@�S�@��@��R@���@��\@�ff@�M�@��@�@��#@���@�@���@�X@���@���@��w@�dZ@�C�@�+@�o@��@��@���@��7@�Q�@� �@�1@�ƨ@�+@���@���@�M�@�{@�@���@�x�@�O�@��@�V@��@��j@�r�@��m@�33@���@�ff@�=q@���@��^@�x�@�`B@�X@�X@�7L@��@�V@���@���@��`@���@��@��@��D@�9X@�ƨ@�|�@�;d@�"�@�o@�
=@���@��@���@��+@�ff@�M�@�=q@�J@��T@���@���@��7@�hs@�O�@�V@�z�@�j@�bN@�Z@�Q�@�I�@�9X@�  @�@��+@�5?@�{@���@��@��@�@���@�&�@��D@�A�@��@
=@~ff@~$�@~@}��@}�h@}`B@}?}@}�@|��@{��@{dZ@{o@z=q@w��@w;d@w+@v�y@vv�@v@u@u`B@t��@t(�@sS�@r��@r�!@r~�@rM�@q�@qx�@qhs@qX@q&�@p�9@pA�@pb@o�@o�P@o;d@n�+@n@m�@l�D@l�D@k��@kC�@j�\@j�@i��@i��@ix�@ihs@iX@iG�@i7L@i&�@i%@h�`@h��@h �@gl�@f@d�@d�D@d9X@c�
@cƨ@c��@c��@c��@c�@c�@cdZ@cS�@c��@c��@c�@cC�@c33@c"�@co@c"�@b�@b��@b^5@a��@aG�@a7L@`��@`��@`�`@`�`@_�@_|�@_K�@_;d@_;d@_;d@_+@_;d@_
=@^��@^��@^�R@^E�@\�@\I�@\(�@[�
@[dZ@[C�@[33@[@Z�\@Z=q@Yx�@XĜ@XbN@W�@W��@W|�@V��@U�T@UV@T�@T��@T��@UV@UV@UV@T��@T�D@Tj@T(�@T1@S�m@Sƨ@S�@SdZ@SdZ@St�@Rn�@R�@QX@Q�@P1'@O�@Ol�@O;d@O+@O
=@N�y@N�R@M�-@K�m@K��@Kt�@J�!@J^5@J�@I�^@I��@IX@H��@H��@H��@H�9@HQ�@H �@H  @G��@G�P@G|�@G|�@G�@G
=@G
=@G
=@G�@G�@F�y@F�R@Fv�@Fff@Fff@Fff@Fff@Fff@F5?@F{@F5?@F5?@E@E`B@E�@C��@C"�@B�H@B��@B��@B=q@A�#@A�^@A��@Ahs@AG�@A%@@�`@@Ĝ@@��@@�u@@�u@@r�@@bN@@Q�@@Q�@@  @?�P@?+@>�@>�R@>�+@>V@>$�@=��@=��@=`B@<�/@<�@<9X@;�@;"�@:��@:M�@:�@:�@:J@9�@9��@9�7@9X@9�@8��@8��@9%@9�@9%@8�9@7�;@7|�@7
=@6��@6@5�@5�T@5@5�h@5p�@5`B@5O�@5V@49X@41@3��@3�m@3ƨ@3��@3S�@3"�@2��@2^5@2M�@2J@1X@0Ĝ@0�@0A�@0  @/��@/|�@/K�@/K�@/;d@/;d@/+@.��@.��@.�+@.ff@.@-@-��@-�@-`B@-V@,��@,Z@,Z@,I�@,�@,�@,1@+t�@*��@*-@)�@)�^@)G�@(��@(��@(�u@(bN@(b@'K�@'�@&��@&�R@&5?@%�T@%��@%�h@%�@%p�@%`B@%O�@%O�@%?}@%/@$��@$��@$�@$�/@$�j@$�@$��@$�D@$Z@$I�@$(�@#��@#�@#S�@#33@#o@"�!@"J@!X@!G�@!&�@!�@ �9@ bN@��@;d@�R@5?@$�@�T@�-@�@O�@��@�@�/@�@��@��@��@�D@z�@z�@z�@j@I�@(�@��@�m@�
@��@33@^5@J@�^@7L@�@��@��@��@��@�u@�@�@r�@r�@bN@Q�@A�@1'@1'@1'@ �@b@b@  @  @�;@l�@
=@ff@/@�j@��@j@Z@Z@Z@I�@��@dZ@33@o@��@-@J@�@�^@X@%@��@�@bN@1'@1'@  @��@�@K�@��@ff@5?@{@{@�@�@��@�-@p�@?}@�j@�D@I�@(�@�@1@ƨ@��@S�@@
n�@	��@	��@	��@	x�@	G�@	�@��@Ĝ@bN@  @�@�;@�@��@��@��@\)@�y@�y@�y@�@�@�@�R@�R@�+@ff@V@5?@{@@@@@@�-@��@��@��@p�@/@�@V@��@��@I�@(�@��@ƨ@��@S�@33@o@�@��@��@=q@��@�#@x�@%@ �`@ Ĝ@ ��@ ��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A�ffA���A��A�{A��
Aա�A�~�A�ffA�K�A�$�A��A�ȴAԮAԣ�Aԗ�Aԇ+A�v�A�hsA�\)A�A�A�9XA�1'A� �A���Aә�A�~�A�dZA�M�A��A�-A�A�A�(�A�ĜA��mA�ĜA��HA�^5A��A��A���A��A��jA�~�A���A��A���A�Q�A�r�A�XA�A��A�A�A��A���A��A�ffA�ĜA���A�ȴA��A���A���A���A���A�5?A���A���A��FA�"�A�`BA�?}A���A�+A���A�A�z�A�%A��PA�oA��RA�`BA��;A�VA�x�A���A�z�A��`A���A�n�A���A� �A��A�1'A�z�A�33A��9A�E�A��A�(�A�v�A��A}�
A}?}Ax �Av�Au
=As|�Ao�#AnA�Al��AkƨAj�Ah�jAg�;AgdZAg;dAf��Ad�Ac��AcoA`�A^�DA[�FAZ�DAZ�AYƨAYO�AXI�AW|�AV�HAV��AVZAU��AS��AR�AO�AOp�AO;dAO33AO+AO"�AOVAN��AN��ANv�AM��AK�AG�#AE�^AEC�AE%AD1AC`BAB�`AB�AA�7A@�!A?��A=��A<��A:�A9��A9t�A9&�A8��A8��A8�9A8��A8Q�A7�mA7VA6JA5t�A4��A3hsA2��A2r�A1�wA1?}A0ffA/�7A.Q�A-&�A,�HA,5?A*jA)`BA(�jA'�A&  A%hsA%
=A$��A$^5A#��A#|�A"�\A ��AȴA?}A�;A33A��A$�AXA  A�yA��A��A��A�HA��A��A^5A5?A�AVA�uA=qAA\)AƨA%A
�yA
ĜA
��A	��A	dZA	oA�jA�AVA�AhsA�jA��At�AoA��AjAXA �/@�"�@��D@��7@�1'@�x�@�j@�v�@�^5@�=q@��-@�`B@��@���@� �@�o@�7L@���@��@��@���@�;d@ᙚ@��u@�l�@�p�@��T@�9X@ׅ@�S�@�=q@Ձ@���@��;@�\)@��@љ�@�`B@��@�A�@��H@�@�%@�S�@ə�@���@�A�@ǍP@�-@Ý�@��@��@�t�@�\)@�C�@�"�@���@���@�~�@��T@�I�@�ff@�@��-@��@�Z@���@�S�@�+@��R@�A�@��y@���@��@��j@�Z@��
@��@�o@���@�{@���@��7@�hs@�&�@��/@�r�@�A�@��@���@���@��P@�t�@�C�@��@��\@��#@���@�A�@���@��R@�E�@���@�hs@�Ĝ@�bN@��@�+@���@��+@�5?@��^@�bN@��@�l�@��@�~�@��@�?}@��9@�z�@�I�@�  @��
@��F@�S�@��@��R@���@��\@�ff@�M�@��@�@��#@���@�@���@�X@���@���@��w@�dZ@�C�@�+@�o@��@��@���@��7@�Q�@� �@�1@�ƨ@�+@���@���@�M�@�{@�@���@�x�@�O�@��@�V@��@��j@�r�@��m@�33@���@�ff@�=q@���@��^@�x�@�`B@�X@�X@�7L@��@�V@���@���@��`@���@��@��@��D@�9X@�ƨ@�|�@�;d@�"�@�o@�
=@���@��@���@��+@�ff@�M�@�=q@�J@��T@���@���@��7@�hs@�O�@�V@�z�@�j@�bN@�Z@�Q�@�I�@�9X@�  @�@��+@�5?@�{@���@��@��@�@���@�&�@��D@�A�@��@
=@~ff@~$�@~@}��@}�h@}`B@}?}@}�@|��@{��@{dZ@{o@z=q@w��@w;d@w+@v�y@vv�@v@u@u`B@t��@t(�@sS�@r��@r�!@r~�@rM�@q�@qx�@qhs@qX@q&�@p�9@pA�@pb@o�@o�P@o;d@n�+@n@m�@l�D@l�D@k��@kC�@j�\@j�@i��@i��@ix�@ihs@iX@iG�@i7L@i&�@i%@h�`@h��@h �@gl�@f@d�@d�D@d9X@c�
@cƨ@c��@c��@c��@c�@c�@cdZ@cS�@c��@c��@c�@cC�@c33@c"�@co@c"�@b�@b��@b^5@a��@aG�@a7L@`��@`��@`�`@`�`@_�@_|�@_K�@_;d@_;d@_;d@_+@_;d@_
=@^��@^��@^�R@^E�@\�@\I�@\(�@[�
@[dZ@[C�@[33@[@Z�\@Z=q@Yx�@XĜ@XbN@W�@W��@W|�@V��@U�T@UV@T�@T��@T��@UV@UV@UV@T��@T�D@Tj@T(�@T1@S�m@Sƨ@S�@SdZ@SdZ@St�@Rn�@R�@QX@Q�@P1'@O�@Ol�@O;d@O+@O
=@N�y@N�R@M�-@K�m@K��@Kt�@J�!@J^5@J�@I�^@I��@IX@H��@H��@H��@H�9@HQ�@H �@H  @G��@G�P@G|�@G|�@G�@G
=@G
=@G
=@G�@G�@F�y@F�R@Fv�@Fff@Fff@Fff@Fff@Fff@F5?@F{@F5?@F5?@E@E`B@E�@C��@C"�@B�H@B��@B��@B=q@A�#@A�^@A��@Ahs@AG�@A%@@�`@@Ĝ@@��@@�u@@�u@@r�@@bN@@Q�@@Q�@@  @?�P@?+@>�@>�R@>�+@>V@>$�@=��@=��@=`B@<�/@<�@<9X@;�@;"�@:��@:M�@:�@:�@:J@9�@9��@9�7@9X@9�@8��@8��@9%@9�@9%@8�9@7�;@7|�@7
=@6��@6@5�@5�T@5@5�h@5p�@5`B@5O�@5V@49X@41@3��@3�m@3ƨ@3��@3S�@3"�@2��@2^5@2M�@2J@1X@0Ĝ@0�@0A�@0  @/��@/|�@/K�@/K�@/;d@/;d@/+@.��@.��@.�+@.ff@.@-@-��@-�@-`B@-V@,��@,Z@,Z@,I�@,�@,�@,1@+t�@*��@*-@)�@)�^@)G�@(��@(��@(�u@(bN@(b@'K�@'�@&��@&�R@&5?@%�T@%��@%�h@%�@%p�@%`B@%O�@%O�@%?}@%/@$��@$��@$�@$�/@$�j@$�@$��@$�D@$Z@$I�@$(�@#��@#�@#S�@#33@#o@"�!@"J@!X@!G�@!&�@!�@ �9@ bN@��@;d@�R@5?@$�@�T@�-@�@O�@��@�@�/@�@��@��@��@�D@z�@z�@z�@j@I�@(�@��@�m@�
@��@33@^5@J@�^@7L@�@��@��@��@��@�u@�@�@r�@r�@bN@Q�@A�@1'@1'@1'@ �@b@b@  @  @�;@l�@
=@ff@/@�j@��@j@Z@Z@Z@I�@��@dZ@33@o@��@-@J@�@�^@X@%@��@�@bN@1'@1'@  @��@�@K�@��@ff@5?@{@{@�@�@��@�-@p�@?}@�j@�D@I�@(�@�@1@ƨ@��@S�@@
n�@	��@	��@	��@	x�@	G�@	�@��@Ĝ@bN@  @�@�;@�@��@��@��@\)@�y@�y@�y@�@�@�@�R@�R@�+@ff@V@5?@{@@@@@@�-@��@��@��@p�@/@�@V@��@��@I�@(�@��@ƨ@��@S�@33@o@�@��@��@=q@��@�#@x�@%@ �`@ Ĝ@ ��@ ��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B'�B'�B(�B)�B$�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B��B��B�dB�B��B��B��B��B��B��B��B��B��B��B�hB�PB�DB�+B�B�B~�Bt�BhsB^5BN�BJ�BF�BC�B?}B>wB<jB7LB"�B�BbB	7B+B��B�B�mB�HB��B�^B�'B��B�PB�%B~�Bv�Bn�BgmB`BBZBR�BL�BG�BA�B5?B"�B�B\B+B
��B
�fB
��B
�^B
�B
�B
��B
��B
��B
�VB
�B
s�B
gmB
T�B
C�B
=qB
$�B
uB
	7B
  B	�B	�BB	�B	��B	ȴB	��B	�dB	�RB	�FB	�3B	�B	��B	��B	��B	�1B	z�B	s�B	p�B	n�B	l�B	hsB	cTB	`BB	^5B	\)B	ZB	S�B	N�B	D�B	@�B	>wB	>wB	=qB	=qB	=qB	<jB	;dB	9XB	5?B	$�B	hB	+B	+B	%B	B	B	B	  B��B��B��B�B�yB�`B�NB�HB�BB�BB�;B�;B�5B�/B�)B�B��B��B��BȴBÖBB�}B�qB�^B�?B�'B�B��B��B��B��B��B�oB�JB�7B�+B�%B�B� By�Bs�Bl�BgmBaHB\)BZBW
BT�BR�BO�BL�BJ�BE�BF�BA�B@�B@�B@�B>wB>wB=qB=qB;dB:^B9XB9XB6FB5?B5?B49B5?B33B2-B2-B1'B1'B0!B0!B/B0!B/B.B-B-B)�B-B)�B(�B'�B&�B&�B&�B$�B$�B#�B#�B"�B"�B"�B"�B#�B#�B#�B$�B$�B%�B%�B&�B&�B&�B'�B+B-B,B,B.B-B.B/B0!B2-B2-B2-B2-B33B6FB6FB8RB:^B=qB>wB?}B?}BB�BG�BO�BO�BO�BO�BO�BP�BP�BP�BQ�BR�BW
B]/B^5B^5B^5BbNBdZBe`Be`BffBn�Bq�Bv�By�By�B{�B}�B~�B� B�B�B�B�%B�%B�+B�1B�=B�DB�DB�PB�PB�VB�VB�\B�bB�hB��B��B��B��B��B��B��B��B�B�B�'B�3B�?B�LB�LB�XB�}BBÖBŢBƨBȴB��B��B��B��B��B��B��B��B�B�
B�
B�
B�B�B�B�B�B�#B�#B�#B�#B�)B�sB�B�B��B��B��B��B��B��B	B		7B	
=B	DB	JB	bB	hB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	#�B	'�B	,B	-B	/B	0!B	2-B	49B	49B	5?B	5?B	6FB	6FB	7LB	7LB	7LB	8RB	8RB	9XB	9XB	:^B	<jB	?}B	B�B	D�B	D�B	E�B	E�B	E�B	F�B	H�B	I�B	J�B	K�B	K�B	M�B	O�B	O�B	O�B	R�B	S�B	T�B	W
B	\)B	\)B	\)B	]/B	]/B	]/B	]/B	`BB	hsB	k�B	m�B	n�B	o�B	o�B	o�B	p�B	p�B	t�B	y�B	|�B	� B	�B	�1B	�7B	�7B	�=B	�DB	�DB	�JB	�JB	�VB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�?B	�FB	�FB	�LB	�XB	�dB	�dB	�dB	�jB	�wB	�}B	��B	��B	B	ÖB	ǮB	ɺB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�5B	�5B	�5B	�5B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�5B	�;B	�;B	�5B	�;B	�5B	�5B	�5B	�5B	�5B	�5B	�;B	�BB	�NB	�ZB	�fB	�mB	�mB	�mB	�mB	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�`B	�`B	�ZB	�ZB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
1B
DB
JB
JB
PB
PB
VB
\B
\B
\B
bB
bB
bB
bB
hB
hB
hB
oB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
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
$�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
(�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
,B
,B
-B
-B
-B
-B
-B
-B
-B
-B
.B
/B
0!B
0!B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
<jB
=qB
=qB
=qB
=qB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
>wB
@�B
A�B
B�B
C�B
C�B
D�B
E�B
E�B
F�B
F�B
G�B
I�B
I�B
I�B
I�B
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
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
S�B
T�B
T�B
T�B
T�B
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
W
B
XB
XB
XB
XB
XB
YB
ZB
ZB
[#B
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
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
_;B
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
cTB
bNB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
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
m�B
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
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
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
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
x�B
x�B
y�B
y�B
y�B
y�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B'�B'�B(�B)�B$�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B��B��B�dB�B��B��B��B��B��B��B��B��B��B��B�hB�PB�DB�+B�B�B~�Bt�BhsB^5BN�BJ�BF�BC�B?}B>wB<jB7LB"�B�BbB	7B+B��B�B�mB�HB��B�^B�'B��B�PB�%B~�Bv�Bn�BgmB`BBZBR�BL�BG�BA�B5?B"�B�B\B+B
��B
�fB
��B
�^B
�B
�B
��B
��B
��B
�VB
�B
s�B
gmB
T�B
C�B
=qB
$�B
uB
	7B
  B	�B	�BB	�B	��B	ȴB	��B	�dB	�RB	�FB	�3B	�B	��B	��B	��B	�1B	z�B	s�B	p�B	n�B	l�B	hsB	cTB	`BB	^5B	\)B	ZB	S�B	N�B	D�B	@�B	>wB	>wB	=qB	=qB	=qB	<jB	;dB	9XB	5?B	$�B	hB	+B	+B	%B	B	B	B	  B��B��B��B�B�yB�`B�NB�HB�BB�BB�;B�;B�5B�/B�)B�B��B��B��BȴBÖBB�}B�qB�^B�?B�'B�B��B��B��B��B��B�oB�JB�7B�+B�%B�B� By�Bs�Bl�BgmBaHB\)BZBW
BT�BR�BO�BL�BJ�BE�BF�BA�B@�B@�B@�B>wB>wB=qB=qB;dB:^B9XB9XB6FB5?B5?B49B5?B33B2-B2-B1'B1'B0!B0!B/B0!B/B.B-B-B)�B-B)�B(�B'�B&�B&�B&�B$�B$�B#�B#�B"�B"�B"�B"�B#�B#�B#�B$�B$�B%�B%�B&�B&�B&�B'�B+B-B,B,B.B-B.B/B0!B2-B2-B2-B2-B33B6FB6FB8RB:^B=qB>wB?}B?}BB�BG�BO�BO�BO�BO�BO�BP�BP�BP�BQ�BR�BW
B]/B^5B^5B^5BbNBdZBe`Be`BffBn�Bq�Bv�By�By�B{�B}�B~�B� B�B�B�B�%B�%B�+B�1B�=B�DB�DB�PB�PB�VB�VB�\B�bB�hB��B��B��B��B��B��B��B��B�B�B�'B�3B�?B�LB�LB�XB�}BBÖBŢBƨBȴB��B��B��B��B��B��B��B��B�B�
B�
B�
B�B�B�B�B�B�#B�#B�#B�#B�)B�sB�B�B��B��B��B��B��B��B	B		7B	
=B	DB	JB	bB	hB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	#�B	'�B	,B	-B	/B	0!B	2-B	49B	49B	5?B	5?B	6FB	6FB	7LB	7LB	7LB	8RB	8RB	9XB	9XB	:^B	<jB	?}B	B�B	D�B	D�B	E�B	E�B	E�B	F�B	H�B	I�B	J�B	K�B	K�B	M�B	O�B	O�B	O�B	R�B	S�B	T�B	W
B	\)B	\)B	\)B	]/B	]/B	]/B	]/B	`BB	hsB	k�B	m�B	n�B	o�B	o�B	o�B	p�B	p�B	t�B	y�B	|�B	� B	�B	�1B	�7B	�7B	�=B	�DB	�DB	�JB	�JB	�VB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�?B	�FB	�FB	�LB	�XB	�dB	�dB	�dB	�jB	�wB	�}B	��B	��B	B	ÖB	ǮB	ɺB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�5B	�5B	�5B	�5B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�5B	�;B	�;B	�5B	�;B	�5B	�5B	�5B	�5B	�5B	�5B	�;B	�BB	�NB	�ZB	�fB	�mB	�mB	�mB	�mB	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�`B	�`B	�ZB	�ZB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
1B
DB
JB
JB
PB
PB
VB
\B
\B
\B
bB
bB
bB
bB
hB
hB
hB
oB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
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
$�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
(�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
,B
,B
-B
-B
-B
-B
-B
-B
-B
-B
.B
/B
0!B
0!B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
<jB
=qB
=qB
=qB
=qB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
>wB
@�B
A�B
B�B
C�B
C�B
D�B
E�B
E�B
F�B
F�B
G�B
I�B
I�B
I�B
I�B
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
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
S�B
T�B
T�B
T�B
T�B
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
W
B
XB
XB
XB
XB
XB
YB
ZB
ZB
[#B
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
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
_;B
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
cTB
bNB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
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
m�B
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
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
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
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
x�B
x�B
y�B
y�B
y�B
y�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                          ! M o ? ) "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          T )�00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000   PRES            TEMP            PSAL            NB_SAMPLE_CTD   PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.17 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230516172242                                          AO  ARCAADJP                                                                    20230516172242    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230516172242  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20230516172242  QCF$                G�O�G�O�G�O�0               