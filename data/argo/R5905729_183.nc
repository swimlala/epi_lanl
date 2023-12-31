CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-05-01T09:00:45Z creation      
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
_FillValue                 �  _   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  r�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �$   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �D   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �d   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �L   NB_SAMPLE_CTD            
         	long_name         2Number of samples in each pressure bin for the CTD     
_FillValue        �     units         count      C_format      %5d    FORTRAN_format        I5     
resolution                �  ��   NB_SAMPLE_CTD_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  @  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  8  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230501090045  20230501090045  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            NB_SAMPLE_CTD      �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @�'ӡ�G1   @�'�:��@+#�e��O�d�3���1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   A       @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/�fD0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5y�D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?y�D?��D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DFfDF�fDG  DGy�DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ Dɼ�D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D��3D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HBz�B'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B��B��B��B��B��B��B��B��B��B��B��qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�Dw�D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/�zD/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5w�D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?w�D?��D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DFzDF�zDF�DGw�DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��=D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D¿
D��
D�?
D�
Dÿ
D��
D�?
D�
DĿ
D��
D�?
D�
Dſ
D��
D�?
D�
Dƿ
D��
D�?
D�
Dǿ
D��
D�?
D�
Dȿ
D��
D�?
D�
Dɻ�D��
D�?
D�
Dʿ
D��
D�?
D�
D˿
D��
D�?
D�
D̿
D��
D�?
D�
DͿ
D��
D�?
D�
Dο
D��
D�?
D�
DϿ
D��
D�?
D�
Dп
D��
D�?
D�
Dѿ
D��
D�?
D�
Dҿ
D��
D�?
D�
Dӿ
D��
D�?
D�
DԿ
D��
D�?
D�
Dտ
D��
D�?
D�
D��=D��
D�?
D�
D׿
D��
D�?
D�
Dؿ
D��
D�?
D�
Dٿ
D��
D�?
D�
Dڿ
D��
D�?
D�
Dۿ
D��
D�?
D�
Dܿ
D��
D�?
D�
Dݿ
D��
D�?
D�
D޿
D��
D�?
D�
D߿
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D��
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��=D�=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�K�A�M�A�M�A�E�A�E�A�E�A�K�A�G�A�O�A�Q�A�S�A�VA�XA�XA�\)A�\)A�S�A�M�A�Q�A�XA�VA�VA�A�A�7LA�-A��A�VA���A��Aإ�Aװ!A�ffA�1'A�AּjA։7A�9XA�C�A�
=A��A�E�A�jA��mA�hsA�x�Aǟ�A��A��mA�%A�n�A�XA��
A�C�A���A�
=A���A��^A��wA��RA���A���A�n�A���A�hsA��#A�I�A�ZA�=qA��RA�1'A��+A�l�A�bNA���A�z�A��A�dZA~A�AzbAw�Au��At-ArVAn�/Al^5Ai��Ae|�A\��A[p�AZ��AY��AY�AX��AW��AWK�AVĜAU�#AS�PAO��AM�AL��AJ�DAF�/AC
=AAC�A@1'A=�TA<�\A9ƨA7�hA6n�A2�A1oA0�9A0-A/��A.�A-\)A+��A*�!A)�A)+A(��A(A'�A&�\A%��A%�FA$�A#�#A#C�A"�uA!�A!XA v�A�A�AjA�#A\)A��AZA��A��A$�A�A�#A��A\)A��A��Ar�A-AJA��A�7A�AoA��AbNA$�A�#A��A��AO�AO�AoA�A��AbNA�A�#A��Al�A��A�9A{A��A;dA��A��A�^A�^A�-A\)A�A5?A�A7LA
=A�yA�AȴA�uA�A~�A�AdZA
��A
��A
��A
�A	��A	\)A	?}A�yA�!A^5A�Ax�A+A��A�/A�!AbNAƨA�Al�AXAK�AG�A7LA�A%A�/A��AffA�A\)AC�A+A�jA-AbA�mA�At�A �`A M�A b@��@�dZ@�S�@�C�@�+@���@��@�bN@�o@�n�@���@�?}@��j@��u@�b@�S�@���@�M�@��#@��@��9@���@���@��u@��
@�!@�$�@��@�p�@���@�j@�|�@�x�@� �@�@�P@�\)@�33@��@�+@�@�%@�u@�z�@��
@�@�X@��@�b@㕁@�J@��/@�j@޸R@�@��/@�|�@��@�G�@؛�@���@�l�@�
=@��y@���@���@֟�@�v�@��T@��`@���@�+@ҏ\@ёh@�?}@�&�@��@��@��@мj@�r�@�1'@��m@�ƨ@ϥ�@ϝ�@ϕ�@�|�@�K�@��y@�E�@�hs@��@��@˝�@ʇ+@���@�`B@�?}@ȓu@�1'@��@���@Ƨ�@Ɵ�@Ɨ�@�ff@��@š�@�G�@Ĵ9@�Q�@��@��
@å�@�t�@�;d@�o@��@°!@�ff@�-@�J@��@��^@�hs@�V@���@��/@���@��9@���@�Q�@� �@�1@��;@���@���@���@���@�dZ@�@���@�V@�=q@�$�@�J@�@�V@���@�1'@� �@�  @��
@��@��y@�ȴ@�ȴ@���@�@���@�hs@�%@��@��D@�I�@�9X@�9X@� �@�\)@���@�n�@��7@���@��;@���@�|�@�l�@�"�@��!@��\@��@��@��D@�Z@��@���@��m@��@��@��!@��\@�ff@���@�X@�bN@��w@�t�@���@�V@�$�@�@��#@���@�?}@��@�A�@�33@�{@���@�hs@�/@���@�1'@��@��@���@�dZ@�;d@��@��y@���@��R@���@�M�@��7@��j@�z�@�9X@���@�ƨ@��F@��@��@���@���@��@�S�@�33@�o@��@�V@���@��@�G�@�V@���@��@���@�ƨ@���@���@��P@��@��H@���@���@���@�@�X@�%@��j@���@�j@�bN@�Z@�A�@� �@��@��;@���@��w@��@���@�C�@�n�@�@��7@�O�@�V@��@�z�@�Z@�9X@���@�K�@�K�@���@��H@�ȴ@���@�~�@�n�@�=q@��@��-@�O�@���@�j@�9X@� �@��m@�t�@�+@��@���@�E�@���@��7@�?}@��@�I�@��@��m@���@�;d@���@��y@���@�5?@���@�@�p�@���@�b@��F@�dZ@�o@���@�v�@��@���@�hs@��@��@��/@���@�Ĝ@���@�r�@��m@�l�@�"�@�~�@��@�@��@�`B@�?}@��@���@�z�@�A�@�  @�w@|�@;d@~�y@~�R@~�+@~ff@}�-@|��@|z�@|(�@{��@{�
@{��@{S�@z�@z�\@zM�@y�#@yhs@y7L@x�`@xĜ@xQ�@x1'@w��@w�@v�+@v@up�@t�j@t(�@sS�@r��@q��@q��@qx�@qG�@q%@p��@p  @o\)@o;d@o
=@n�+@m@mp�@mV@l�/@l�@lZ@l(�@k��@kt�@k33@j�!@jM�@j-@i��@i��@iG�@h�9@h  @g
=@f��@f�+@ep�@d��@dZ@d�@c��@cC�@b�!@bJ@a��@a%@`�u@`1'@_�w@_l�@_�@^@]?}@\��@\��@\Z@[�F@[dZ@Z��@ZJ@Yx�@YX@YG�@Y&�@X��@X��@Xr�@XQ�@X1'@X1'@W�@Wl�@W+@W
=@V�@V��@V�+@Vv�@VV@V5?@V{@U�T@U��@U`B@U/@U�@UV@T�/@T9X@S�m@St�@R�@Q�7@QG�@Q7L@Q�@PĜ@P1'@O��@O;d@O
=@N�y@Nȴ@NE�@M�@M?}@L�/@Lj@L1@L1@K�m@K��@KdZ@Ko@K@J��@J^5@I�^@I��@IX@I%@H�@G�@G;d@F�R@FV@F{@Ep�@D��@D�j@D��@DI�@C�
@C��@Co@B��@B-@A�^@AG�@@�9@@Q�@@r�@@r�@@1'@?�P@?�@>ȴ@>�+@>E�@>{@=@=`B@<�@<z�@<�@;�
@;��@;o@:��@:��@:��@:�!@:��@:~�@:�@9��@9x�@9x�@9x�@9hs@9%@8�u@8Q�@81'@7�@7��@7l�@7;d@7+@7�@6�y@6ȴ@6ȴ@6��@6�+@6E�@5�T@5�h@5/@4�@4��@4Z@3��@3�F@3�@3t�@333@2�@2�!@2^5@2-@1hs@1�@0��@0r�@0  @/l�@/K�@/K�@/
=@.ȴ@.�+@.$�@.$�@.@-��@-�h@-O�@-V@,��@,Z@,j@,Z@,I�@,1@+��@+�m@+�m@+�
@+�F@+t�@+t�@+S�@+S�@+33@+"�@+@*�@*�H@*�H@*�H@*�!@)��@)x�@)X@(��@(r�@(A�@'�;@'�@'�P@'|�@'l�@'l�@'l�@'|�@'\)@'
=@&�R@&V@%�T@%�@$��@$��@#��@#dZ@"�@"��@"�\@"-@"�@"�@"J@!��@!x�@!X@!%@ �`@ ��@ �u@ Q�@ 1'@ 1'@ b@�;@��@�w@|�@��@ȴ@��@v�@V@E�@@�T@@�-@�@O�@/@�@�@9X@�@�
@�@��@n�@^5@^5@�@��@x�@X@�`@�@r�@bN@b@�;@�;@��@�@l�@\)@;d@�@�y@ȴ@�R@��@�+@E�@�@�@O�@?}@/@�@��@z�@j@Z@I�@I�@9X@9X@9X@9X@9X@(�@ƨ@��@S�@�H@�H@��@�!@�\@-@J@�@��@�^@��@G�@&�@�@��@�9@�@A�@  @  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�K�A�M�A�M�A�E�A�E�A�E�A�K�A�G�A�O�A�Q�A�S�A�VA�XA�XA�\)A�\)A�S�A�M�A�Q�A�XA�VA�VA�A�A�7LA�-A��A�VA���A��Aإ�Aװ!A�ffA�1'A�AּjA։7A�9XA�C�A�
=A��A�E�A�jA��mA�hsA�x�Aǟ�A��A��mA�%A�n�A�XA��
A�C�A���A�
=A���A��^A��wA��RA���A���A�n�A���A�hsA��#A�I�A�ZA�=qA��RA�1'A��+A�l�A�bNA���A�z�A��A�dZA~A�AzbAw�Au��At-ArVAn�/Al^5Ai��Ae|�A\��A[p�AZ��AY��AY�AX��AW��AWK�AVĜAU�#AS�PAO��AM�AL��AJ�DAF�/AC
=AAC�A@1'A=�TA<�\A9ƨA7�hA6n�A2�A1oA0�9A0-A/��A.�A-\)A+��A*�!A)�A)+A(��A(A'�A&�\A%��A%�FA$�A#�#A#C�A"�uA!�A!XA v�A�A�AjA�#A\)A��AZA��A��A$�A�A�#A��A\)A��A��Ar�A-AJA��A�7A�AoA��AbNA$�A�#A��A��AO�AO�AoA�A��AbNA�A�#A��Al�A��A�9A{A��A;dA��A��A�^A�^A�-A\)A�A5?A�A7LA
=A�yA�AȴA�uA�A~�A�AdZA
��A
��A
��A
�A	��A	\)A	?}A�yA�!A^5A�Ax�A+A��A�/A�!AbNAƨA�Al�AXAK�AG�A7LA�A%A�/A��AffA�A\)AC�A+A�jA-AbA�mA�At�A �`A M�A b@��@�dZ@�S�@�C�@�+@���@��@�bN@�o@�n�@���@�?}@��j@��u@�b@�S�@���@�M�@��#@��@��9@���@���@��u@��
@�!@�$�@��@�p�@���@�j@�|�@�x�@� �@�@�P@�\)@�33@��@�+@�@�%@�u@�z�@��
@�@�X@��@�b@㕁@�J@��/@�j@޸R@�@��/@�|�@��@�G�@؛�@���@�l�@�
=@��y@���@���@֟�@�v�@��T@��`@���@�+@ҏ\@ёh@�?}@�&�@��@��@��@мj@�r�@�1'@��m@�ƨ@ϥ�@ϝ�@ϕ�@�|�@�K�@��y@�E�@�hs@��@��@˝�@ʇ+@���@�`B@�?}@ȓu@�1'@��@���@Ƨ�@Ɵ�@Ɨ�@�ff@��@š�@�G�@Ĵ9@�Q�@��@��
@å�@�t�@�;d@�o@��@°!@�ff@�-@�J@��@��^@�hs@�V@���@��/@���@��9@���@�Q�@� �@�1@��;@���@���@���@���@�dZ@�@���@�V@�=q@�$�@�J@�@�V@���@�1'@� �@�  @��
@��@��y@�ȴ@�ȴ@���@�@���@�hs@�%@��@��D@�I�@�9X@�9X@� �@�\)@���@�n�@��7@���@��;@���@�|�@�l�@�"�@��!@��\@��@��@��D@�Z@��@���@��m@��@��@��!@��\@�ff@���@�X@�bN@��w@�t�@���@�V@�$�@�@��#@���@�?}@��@�A�@�33@�{@���@�hs@�/@���@�1'@��@��@���@�dZ@�;d@��@��y@���@��R@���@�M�@��7@��j@�z�@�9X@���@�ƨ@��F@��@��@���@���@��@�S�@�33@�o@��@�V@���@��@�G�@�V@���@��@���@�ƨ@���@���@��P@��@��H@���@���@���@�@�X@�%@��j@���@�j@�bN@�Z@�A�@� �@��@��;@���@��w@��@���@�C�@�n�@�@��7@�O�@�V@��@�z�@�Z@�9X@���@�K�@�K�@���@��H@�ȴ@���@�~�@�n�@�=q@��@��-@�O�@���@�j@�9X@� �@��m@�t�@�+@��@���@�E�@���@��7@�?}@��@�I�@��@��m@���@�;d@���@��y@���@�5?@���@�@�p�@���@�b@��F@�dZ@�o@���@�v�@��@���@�hs@��@��@��/@���@�Ĝ@���@�r�@��m@�l�@�"�@�~�@��@�@��@�`B@�?}@��@���@�z�@�A�@�  @�w@|�@;d@~�y@~�R@~�+@~ff@}�-@|��@|z�@|(�@{��@{�
@{��@{S�@z�@z�\@zM�@y�#@yhs@y7L@x�`@xĜ@xQ�@x1'@w��@w�@v�+@v@up�@t�j@t(�@sS�@r��@q��@q��@qx�@qG�@q%@p��@p  @o\)@o;d@o
=@n�+@m@mp�@mV@l�/@l�@lZ@l(�@k��@kt�@k33@j�!@jM�@j-@i��@i��@iG�@h�9@h  @g
=@f��@f�+@ep�@d��@dZ@d�@c��@cC�@b�!@bJ@a��@a%@`�u@`1'@_�w@_l�@_�@^@]?}@\��@\��@\Z@[�F@[dZ@Z��@ZJ@Yx�@YX@YG�@Y&�@X��@X��@Xr�@XQ�@X1'@X1'@W�@Wl�@W+@W
=@V�@V��@V�+@Vv�@VV@V5?@V{@U�T@U��@U`B@U/@U�@UV@T�/@T9X@S�m@St�@R�@Q�7@QG�@Q7L@Q�@PĜ@P1'@O��@O;d@O
=@N�y@Nȴ@NE�@M�@M?}@L�/@Lj@L1@L1@K�m@K��@KdZ@Ko@K@J��@J^5@I�^@I��@IX@I%@H�@G�@G;d@F�R@FV@F{@Ep�@D��@D�j@D��@DI�@C�
@C��@Co@B��@B-@A�^@AG�@@�9@@Q�@@r�@@r�@@1'@?�P@?�@>ȴ@>�+@>E�@>{@=@=`B@<�@<z�@<�@;�
@;��@;o@:��@:��@:��@:�!@:��@:~�@:�@9��@9x�@9x�@9x�@9hs@9%@8�u@8Q�@81'@7�@7��@7l�@7;d@7+@7�@6�y@6ȴ@6ȴ@6��@6�+@6E�@5�T@5�h@5/@4�@4��@4Z@3��@3�F@3�@3t�@333@2�@2�!@2^5@2-@1hs@1�@0��@0r�@0  @/l�@/K�@/K�@/
=@.ȴ@.�+@.$�@.$�@.@-��@-�h@-O�@-V@,��@,Z@,j@,Z@,I�@,1@+��@+�m@+�m@+�
@+�F@+t�@+t�@+S�@+S�@+33@+"�@+@*�@*�H@*�H@*�H@*�!@)��@)x�@)X@(��@(r�@(A�@'�;@'�@'�P@'|�@'l�@'l�@'l�@'|�@'\)@'
=@&�R@&V@%�T@%�@$��@$��@#��@#dZ@"�@"��@"�\@"-@"�@"�@"J@!��@!x�@!X@!%@ �`@ ��@ �u@ Q�@ 1'@ 1'@ b@�;@��@�w@|�@��@ȴ@��@v�@V@E�@@�T@@�-@�@O�@/@�@�@9X@�@�
@�@��@n�@^5@^5@�@��@x�@X@�`@�@r�@bN@b@�;@�;@��@�@l�@\)@;d@�@�y@ȴ@�R@��@�+@E�@�@�@O�@?}@/@�@��@z�@j@Z@I�@I�@9X@9X@9X@9X@9X@(�@ƨ@��@S�@�H@�H@��@�!@�\@-@J@�@��@�^@��@G�@&�@�@��@�9@�@A�@  @  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
1B
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
1B
	7B
1B
1B
1B
	7B
1B
1B
+B
%B
%B
%B
B
B	��B	�B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
0!B
:^B
z�B
��B
�B
�?B  B�B�B.B5?BT�Bl�B\)Bn�BbNB}�B�B��B|�B��By�BhsBffBQ�B=qB/B%B"�BbB
�}B
s�B
�DB
^5B
!�B
bB	��B	��B	�B	�
B	��B	ȴB	�dB	��B	�B	u�B	[#B	49B��B	I�B	T�B	VB	VB	O�B	K�B	O�B	I�B	@�B	-B	#�B	?}B	H�B	;dB	33B	9XB	XB	hsB	t�B	�%B	� B	v�B	r�B	ZB	l�B	�=B	�1B	�B	� B	z�B	}�B	�bB	��B	��B	�B	�B	�-B	�qB	B	ǮB	ƨB	ƨB	�B	�#B	�)B	�HB	�BB	�mB	�B	�B	�B	�B	�B	�B	��B	��B

=B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
#�B
�B
�B
!�B
&�B
'�B
-B
49B
8RB
=qB
:^B
:^B
8RB
8RB
8RB
:^B
=qB
;dB
<jB
=qB
;dB
=qB
?}B
>wB
>wB
L�B
O�B
Q�B
N�B
H�B
I�B
F�B
H�B
M�B
O�B
Q�B
VB
XB
ZB
W
B
Q�B
K�B
P�B
Q�B
O�B
I�B
H�B
J�B
L�B
I�B
J�B
H�B
F�B
G�B
I�B
L�B
M�B
L�B
J�B
F�B
J�B
M�B
M�B
L�B
M�B
K�B
J�B
I�B
G�B
E�B
D�B
@�B
?}B
E�B
D�B
?}B
<jB
B�B
@�B
>wB
<jB
7LB
6FB
;dB
;dB
?}B
?}B
=qB
<jB
8RB
2-B
-B
.B
1'B
2-B
33B
33B
49B
1'B
.B
-B
/B
/B
,B
.B
1'B
0!B
-B
'�B
$�B
'�B
+B
(�B
&�B
%�B
!�B
�B
$�B
-B
0!B
0!B
/B
.B
)�B
'�B
(�B
+B
+B
$�B
�B
!�B
"�B
�B
�B
{B
bB
�B
�B
oB
�B
oB
�B
bB
{B
�B
�B
�B
�B
�B
�B
�B
�B
oB
VB
\B
bB
oB
bB
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
{B
oB
\B
PB
DB
\B
VB
%B
B
DB
PB
VB
DB
JB
	7B
bB
uB
{B
oB
bB
\B
PB
bB
VB
oB
hB
{B
uB
uB
uB
{B
{B
uB
uB
{B
�B
{B
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
"�B
!�B
�B
�B
�B
�B
�B
"�B
$�B
$�B
#�B
!�B
$�B
%�B
&�B
%�B
&�B
&�B
&�B
&�B
&�B
%�B
#�B
 �B
 �B
%�B
&�B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
(�B
(�B
(�B
'�B
&�B
#�B
#�B
'�B
'�B
'�B
&�B
$�B
(�B
,B
+B
+B
+B
'�B
)�B
+B
(�B
%�B
)�B
)�B
+B
,B
/B
.B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
0!B
0!B
.B
+B
'�B
)�B
/B
/B
/B
/B
0!B
1'B
0!B
/B
0!B
49B
2-B
49B
49B
33B
49B
49B
33B
33B
1'B
1'B
1'B
2-B
5?B
6FB
5?B
49B
5?B
6FB
6FB
49B
5?B
49B
6FB
6FB
7LB
;dB
;dB
;dB
:^B
<jB
=qB
<jB
9XB
:^B
;dB
9XB
8RB
9XB
;dB
<jB
=qB
=qB
=qB
<jB
?}B
?}B
@�B
B�B
E�B
E�B
E�B
D�B
C�B
A�B
A�B
C�B
B�B
C�B
H�B
H�B
I�B
I�B
I�B
H�B
F�B
I�B
I�B
K�B
K�B
K�B
K�B
L�B
L�B
K�B
I�B
J�B
L�B
M�B
N�B
N�B
N�B
M�B
M�B
M�B
N�B
M�B
N�B
O�B
O�B
O�B
N�B
O�B
N�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
P�B
P�B
S�B
R�B
S�B
S�B
S�B
R�B
R�B
VB
T�B
S�B
S�B
T�B
VB
W
B
VB
VB
VB
VB
T�B
VB
VB
VB
W
B
W
B
VB
VB
T�B
T�B
T�B
W
B
YB
VB
W
B
YB
ZB
ZB
ZB
ZB
ZB
[#B
ZB
[#B
\)B
\)B
\)B
\)B
ZB
\)B
^5B
^5B
^5B
]/B
^5B
]/B
^5B
_;B
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
aHB
`BB
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
cTB
cTB
cTB
dZB
cTB
cTB
aHB
bNB
cTB
dZB
bNB
ffB
gmB
ffB
ffB
dZB
e`B
ffB
gmB
gmB
ffB
e`B
e`B
gmB
gmB
gmB
gmB
iyB
hsB
hsB
hsB
hsB
iyB
iyB
hsB
gmB
iyB
iyB
hsB
hsB
gmB
hsB
iyB
iyB
jB
iyB
jB
jB
l�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
m�B
m�B
n�B
p�B
p�B
o�B
n�B
n�B
o�B
p�B
p�B
q�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
s�B
t�B
t�B
s�B
s�B
r�B
s�B
t�B
u�B
u�B
t�B
s�B
s�B
t�B
u�B
u�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
u�B
u�B
v�B
v�B
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
w�B
x�B
y�B
y�B
x�B
y�B
z�B
{�B
z�B
z�B
z�B
{�B
|�B
{�B
{�B
{�B
{�B
{�B
z�B
{�B
|�B
|�B
|�B
|�B
}�B
~�B
~�B
~�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
}�B
{�B
}�B
~�B
}�B
}�B
~�B
~�B
� B
�B
�B
�B
�B
�B
�B
� B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
}�B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�B
�B
�%B
�%B
�%B
�%B
�B
�+B
�7B
�1B
�+B
�1B
�1B
�1B
�1B
�1B
�=B
�=B
�7B
�=B
�DB
�DB
�=B
�=B
�DB
�DB
�JB
�DB
�JB
�JB
�JB
�JB
�DB
�DB
�DB
�PB
�VB
�VB
�PB
�PB
�VB
�bB
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�hB
�bB
�\B
�hB
�bB
�bB
�oB
�oB
�hB
�hB
�bB
�oB
�oB
�oB
�oB
�oB
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�uB
�{B
�{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
1B
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
1B
	7B
1B
1B
1B
	7B
1B
1B
+B
%B
%B
%B
B
B	��B	�B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
0!B
:^B
z�B
��B
�B
�?B  B�B�B.B5?BT�Bl�B\)Bn�BbNB}�B�B��B|�B��By�BhsBffBQ�B=qB/B%B"�BbB
�}B
s�B
�DB
^5B
!�B
bB	��B	��B	�B	�
B	��B	ȴB	�dB	��B	�B	u�B	[#B	49B��B	I�B	T�B	VB	VB	O�B	K�B	O�B	I�B	@�B	-B	#�B	?}B	H�B	;dB	33B	9XB	XB	hsB	t�B	�%B	� B	v�B	r�B	ZB	l�B	�=B	�1B	�B	� B	z�B	}�B	�bB	��B	��B	�B	�B	�-B	�qB	B	ǮB	ƨB	ƨB	�B	�#B	�)B	�HB	�BB	�mB	�B	�B	�B	�B	�B	�B	��B	��B

=B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
#�B
�B
�B
!�B
&�B
'�B
-B
49B
8RB
=qB
:^B
:^B
8RB
8RB
8RB
:^B
=qB
;dB
<jB
=qB
;dB
=qB
?}B
>wB
>wB
L�B
O�B
Q�B
N�B
H�B
I�B
F�B
H�B
M�B
O�B
Q�B
VB
XB
ZB
W
B
Q�B
K�B
P�B
Q�B
O�B
I�B
H�B
J�B
L�B
I�B
J�B
H�B
F�B
G�B
I�B
L�B
M�B
L�B
J�B
F�B
J�B
M�B
M�B
L�B
M�B
K�B
J�B
I�B
G�B
E�B
D�B
@�B
?}B
E�B
D�B
?}B
<jB
B�B
@�B
>wB
<jB
7LB
6FB
;dB
;dB
?}B
?}B
=qB
<jB
8RB
2-B
-B
.B
1'B
2-B
33B
33B
49B
1'B
.B
-B
/B
/B
,B
.B
1'B
0!B
-B
'�B
$�B
'�B
+B
(�B
&�B
%�B
!�B
�B
$�B
-B
0!B
0!B
/B
.B
)�B
'�B
(�B
+B
+B
$�B
�B
!�B
"�B
�B
�B
{B
bB
�B
�B
oB
�B
oB
�B
bB
{B
�B
�B
�B
�B
�B
�B
�B
�B
oB
VB
\B
bB
oB
bB
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
{B
oB
\B
PB
DB
\B
VB
%B
B
DB
PB
VB
DB
JB
	7B
bB
uB
{B
oB
bB
\B
PB
bB
VB
oB
hB
{B
uB
uB
uB
{B
{B
uB
uB
{B
�B
{B
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
"�B
!�B
�B
�B
�B
�B
�B
"�B
$�B
$�B
#�B
!�B
$�B
%�B
&�B
%�B
&�B
&�B
&�B
&�B
&�B
%�B
#�B
 �B
 �B
%�B
&�B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
(�B
(�B
(�B
'�B
&�B
#�B
#�B
'�B
'�B
'�B
&�B
$�B
(�B
,B
+B
+B
+B
'�B
)�B
+B
(�B
%�B
)�B
)�B
+B
,B
/B
.B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
0!B
0!B
.B
+B
'�B
)�B
/B
/B
/B
/B
0!B
1'B
0!B
/B
0!B
49B
2-B
49B
49B
33B
49B
49B
33B
33B
1'B
1'B
1'B
2-B
5?B
6FB
5?B
49B
5?B
6FB
6FB
49B
5?B
49B
6FB
6FB
7LB
;dB
;dB
;dB
:^B
<jB
=qB
<jB
9XB
:^B
;dB
9XB
8RB
9XB
;dB
<jB
=qB
=qB
=qB
<jB
?}B
?}B
@�B
B�B
E�B
E�B
E�B
D�B
C�B
A�B
A�B
C�B
B�B
C�B
H�B
H�B
I�B
I�B
I�B
H�B
F�B
I�B
I�B
K�B
K�B
K�B
K�B
L�B
L�B
K�B
I�B
J�B
L�B
M�B
N�B
N�B
N�B
M�B
M�B
M�B
N�B
M�B
N�B
O�B
O�B
O�B
N�B
O�B
N�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
P�B
P�B
S�B
R�B
S�B
S�B
S�B
R�B
R�B
VB
T�B
S�B
S�B
T�B
VB
W
B
VB
VB
VB
VB
T�B
VB
VB
VB
W
B
W
B
VB
VB
T�B
T�B
T�B
W
B
YB
VB
W
B
YB
ZB
ZB
ZB
ZB
ZB
[#B
ZB
[#B
\)B
\)B
\)B
\)B
ZB
\)B
^5B
^5B
^5B
]/B
^5B
]/B
^5B
_;B
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
aHB
`BB
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
cTB
cTB
cTB
dZB
cTB
cTB
aHB
bNB
cTB
dZB
bNB
ffB
gmB
ffB
ffB
dZB
e`B
ffB
gmB
gmB
ffB
e`B
e`B
gmB
gmB
gmB
gmB
iyB
hsB
hsB
hsB
hsB
iyB
iyB
hsB
gmB
iyB
iyB
hsB
hsB
gmB
hsB
iyB
iyB
jB
iyB
jB
jB
l�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
m�B
m�B
n�B
p�B
p�B
o�B
n�B
n�B
o�B
p�B
p�B
q�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
s�B
t�B
t�B
s�B
s�B
r�B
s�B
t�B
u�B
u�B
t�B
s�B
s�B
t�B
u�B
u�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
u�B
u�B
v�B
v�B
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
w�B
x�B
y�B
y�B
x�B
y�B
z�B
{�B
z�B
z�B
z�B
{�B
|�B
{�B
{�B
{�B
{�B
{�B
z�B
{�B
|�B
|�B
|�B
|�B
}�B
~�B
~�B
~�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
}�B
{�B
}�B
~�B
}�B
}�B
~�B
~�B
� B
�B
�B
�B
�B
�B
�B
� B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
}�B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�B
�B
�%B
�%B
�%B
�%B
�B
�+B
�7B
�1B
�+B
�1B
�1B
�1B
�1B
�1B
�=B
�=B
�7B
�=B
�DB
�DB
�=B
�=B
�DB
�DB
�JB
�DB
�JB
�JB
�JB
�JB
�DB
�DB
�DB
�PB
�VB
�VB
�PB
�PB
�VB
�bB
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�hB
�bB
�\B
�hB
�bB
�bB
�oB
�oB
�hB
�hB
�bB
�oB
�oB
�oB
�oB
�oB
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�uB
�{B
�{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�  ! "                                     % / 4 , * ,        " & &   # "               # "                            " ! "                            ! "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      e�000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 PRES            TEMP            PSAL            NB_SAMPLE_CTD   PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.03 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230501090045                                          AO  ARCAADJP                                                                    20230501090045    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230501090045  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20230501090045  QCF$                G�O�G�O�G�O�0               