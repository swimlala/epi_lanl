CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-09-13T00:35:09Z creation;2017-09-13T00:35:13Z conversion to V3.1;2019-12-19T08:01:49Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ͬ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �P   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �X   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �`   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170913003509  20200115121516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_158                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�%��P 1   @�%��I�@:g�ݗ�+�d��u1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B��B  B   B(  B0  B8  B@  BHffBP  BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk�fDlfDl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� Dz��D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�C3D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D��3D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @/\)@|(�@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�B (�BB\)BBB'B/B7B?BH(�BOBW\)B_BgBoBwBB��HB��HB��HB�{B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C
=C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C`
=Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk��Dl�Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz��D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D�HD�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�:�D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�AHD�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D��HD��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D�HD�G�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aߝ�Aߟ�Aߟ�Aߝ�Aߛ�Aߛ�Aߝ�Aߝ�Aߛ�Aߛ�Aߛ�Aߟ�Aߟ�Aߟ�Aߡ�Aߣ�Aߟ�A�G�A�ȴA�VA�~�A�hsA�C�A٩�A׶FA��
AӼjA�E�A�I�A�M�A�oAŰ!A�oA��A��+A���A�/A��hA�dZA��^A���A���A���A�n�A��A���A�Q�A���A�t�A��A��wA�t�A�(�A�O�A���A�x�A�1'A�A�&�A��FA���A���A��;A�ĜA�;dA�ZA��\A���A��yA��hA�bA�7LA�hsA���A���A��7A��;A��!A�+A�K�A�$�A�A�p�A�\)A��-A��FA�  A�z�A���A���A���A�A��A|�9A{��AzM�AyO�Ay��Ax�Aw�TAwAv-Au�-Au?}As��Arz�AqAp�AoS�Am��Akl�AkG�Ak;dAk
=Ajn�Ah��Ag�TAg�AgS�Ag&�Af~�Ae�hAe`BAe%Ac�;AcO�Aa�
A`�/A_�7A^~�A]K�A\A[�A[�AZ{AYƨAY;dAX��AXM�AXbAWƨAWAVE�AT��ASƨAR1AQ��APĜAO+AN�/AN��AN  AM?}AL��AL-AK��AJ�uAI33AHbNAGG�AFZAF�AE��AD^5AC�mAC��AC�7AB��AA�A@A�A?��A>jA=t�A<�`A<bNA;��A:(�A9p�A9�A8�A8  A7l�A6�`A6E�A5hsA5�A3��A2��A25?A1&�A0I�A0{A0A/��A/�A/��A/��A/33A.��A,M�A+�7A+G�A*�A)��A(��A&jA%��A%�A$ �A"bNA!��A!�PA!
=Ax�A{A|�A�FA�!An�AA�A�AƨA�AffAC�Az�A��A?}A��A=qA��AG�A
=AA�A�A��A`BA��A�#A
�`A
~�A
A�yA��A��A�7AhsA�A�/AjA$�A�TA��A�AXA��A�A��AdZA%A��A��A �@��@�^5@�{@�`B@��D@�1@�  @��F@�33@��\@�{@�K�@�S�@���@�9X@�=q@�F@旍@� �@�;d@���@���@�h@��u@���@߾w@߶F@�;d@���@�1@�=q@؛�@�33@�V@�x�@�S�@��@҇+@�7L@υ@�"�@��@�=q@ˮ@��@Ǖ�@�;d@�o@Ƈ+@��T@�7L@�z�@��@�\)@�^5@��@�V@�  @���@�%@���@�o@�V@�V@�M�@�bN@�9X@��y@�j@�l�@���@�@���@�7L@���@��j@�(�@�^5@�x�@�%@��`@��@�j@� �@��F@�l�@��@�$�@�?}@� �@���@�dZ@�33@�33@�;d@�@���@���@�-@�hs@��9@��@���@��P@�+@��@���@��!@���@�@�G�@��j@�Q�@�dZ@���@�~�@�n�@�-@���@���@��7@���@�r�@��;@���@��@��y@���@�5?@��T@��h@�p�@��@��j@�r�@�I�@�1'@��;@���@�l�@�"�@���@�$�@���@�p�@�O�@��@�Ĝ@��@�bN@�(�@��;@��w@�l�@���@�Ĝ@�bN@�I�@�b@���@�dZ@�o@��H@�ȴ@���@�n�@�5?@��@�`B@�j@���@�"�@��y@��!@���@�n�@�=q@�$�@�{@���@���@�%@�1'@��m@��F@���@��P@�l�@�33@�+@�o@��@��!@�V@��@���@��h@��`@��@�w@~{@|��@|9X@{33@yG�@xA�@v�+@v@u�T@u��@u��@u@u�-@u`B@tj@t�@sƨ@s@rn�@q�@q�^@q��@qG�@pr�@pA�@pb@o��@ol�@o;d@n�@nE�@m@m�h@m�@l�@l9X@k�
@k�F@k�F@k��@k"�@j��@jn�@j�@i�@i��@ihs@iX@iG�@i7L@h��@h��@h��@g�@gK�@g
=@f�+@e�@e@e�-@e�-@e�h@d��@d(�@d�@d1@c��@co@b~�@a�@a��@aX@`��@`�@`1'@_��@_�@_|�@_\)@_+@^�@^��@^��@^�+@^{@]�@\�/@\�@[��@[dZ@["�@Z�@Z��@Z�@Y�7@YG�@X�`@W�@WK�@Vȴ@V5?@U@U`B@U�@T�@T�D@Tj@TI�@S�m@S�F@S�@R��@R^5@Q�^@PQ�@O�@O|�@Ol�@OK�@N�y@N�@Nȴ@N��@Nff@N5?@N$�@M��@M/@MV@L�j@K��@K@J�!@J�\@J=q@I��@IG�@I�@H�u@G�;@G|�@F�y@F5?@E�@E��@E?}@E�@D��@D1@CC�@Co@B�!@B-@A��@Ahs@AX@AG�@A&�@A%@@�`@@Ĝ@@�@?��@>��@>�@>�R@>�R@>��@>��@>v�@>v�@>5?@=�-@=`B@=/@<��@<�@<��@<z�@<(�@;�
@;dZ@;S�@;o@:n�@:J@9��@9�#@9�#@9�#@9��@9X@9�@8��@8Ĝ@8�u@8b@7��@7|�@7|�@7l�@7+@7
=@6�@6�R@6�+@6v�@6ff@6E�@6E�@6@5��@5V@4�@4I�@3ƨ@3�@3dZ@333@3o@2�H@2�!@2n�@2M�@2J@2J@1�@1�^@1�^@1��@1��@1x�@1X@17L@0��@0�9@0bN@0 �@/�@/�@/l�@.��@.�y@.��@.��@.v�@.v�@.v�@.ff@.$�@.{@.{@.@-��@-�-@-��@-�h@-`B@,�@,�D@,�D@,9X@+ƨ@+C�@*��@*�\@*M�@*=q@*�@)��@)�^@)x�@)7L@(Ĝ@(�u@(�@(bN@(1'@(1'@(1'@(1'@( �@(  @'�@'l�@';d@&��@&ȴ@&ȴ@&��@&�+@&V@&5?@&{@%�@%@%�-@%�@$��@$�/@$��@$I�@#�m@#ƨ@#�F@#�@#C�@#"�@"��@"��@"�\@"^5@"=q@"J@!�@!�^@!X@!&�@ Ĝ@ �@ Q�@ b@�;@�P@;d@��@�R@��@��@V@�@�@/@��@�@��@j@�
@dZ@o@@��@M�@J@��@��@hs@��@��@��@�u@�@1'@�;@��@�@;d@�R@��@��@ff@{@�T@�T@@�h@�@�@`B@`B@`B@O�@�@V@�@z�@�@�m@�
@ƨ@S�@~�@M�@J@��@�@�#@��@��@hs@X@&�@�9@r�@bN@Q�@Q�@A�@1'@  @��@�w@��@K�@�y@��@ff@{@@�@@p�@O�@?}@/@V@�@�/@�@��@�D@Z@(�@��@ƨ@��@t�@"�@
��@
��@
n�@
=q@	�@	��@	hs@	x�@	G�@	�@	�@��@�`@��@��@�`@Ĝ@�@bN@bN@bN@bN@Q�@ �@��@�@l�@�@
=@�y@�y@�y@�@ȴ@��@E�@$�@{@{@@�@��@�T@�T@�-@p�@O�@/@/@V@��@�@��@�j@��@z�@(�@ƨ@�F@t�@�@��@~�@^5@=q@�@J@��@��@�@��@��@x�@G�@7L@&�@ �`@ ��@ Ĝ@ ��@ ��@ �@ bN@ Q�@ Q�@ Q�@ Q�@ Q�@ A�@ 1'@  �?��;?��w?���?���?�v�?�5??�{?��?��?��-?�V?�V?��?��?���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aߝ�Aߟ�Aߟ�Aߝ�Aߛ�Aߛ�Aߝ�Aߝ�Aߛ�Aߛ�Aߛ�Aߟ�Aߟ�Aߟ�Aߡ�Aߣ�Aߟ�A�G�A�ȴA�VA�~�A�hsA�C�A٩�A׶FA��
AӼjA�E�A�I�A�M�A�oAŰ!A�oA��A��+A���A�/A��hA�dZA��^A���A���A���A�n�A��A���A�Q�A���A�t�A��A��wA�t�A�(�A�O�A���A�x�A�1'A�A�&�A��FA���A���A��;A�ĜA�;dA�ZA��\A���A��yA��hA�bA�7LA�hsA���A���A��7A��;A��!A�+A�K�A�$�A�A�p�A�\)A��-A��FA�  A�z�A���A���A���A�A��A|�9A{��AzM�AyO�Ay��Ax�Aw�TAwAv-Au�-Au?}As��Arz�AqAp�AoS�Am��Akl�AkG�Ak;dAk
=Ajn�Ah��Ag�TAg�AgS�Ag&�Af~�Ae�hAe`BAe%Ac�;AcO�Aa�
A`�/A_�7A^~�A]K�A\A[�A[�AZ{AYƨAY;dAX��AXM�AXbAWƨAWAVE�AT��ASƨAR1AQ��APĜAO+AN�/AN��AN  AM?}AL��AL-AK��AJ�uAI33AHbNAGG�AFZAF�AE��AD^5AC�mAC��AC�7AB��AA�A@A�A?��A>jA=t�A<�`A<bNA;��A:(�A9p�A9�A8�A8  A7l�A6�`A6E�A5hsA5�A3��A2��A25?A1&�A0I�A0{A0A/��A/�A/��A/��A/33A.��A,M�A+�7A+G�A*�A)��A(��A&jA%��A%�A$ �A"bNA!��A!�PA!
=Ax�A{A|�A�FA�!An�AA�A�AƨA�AffAC�Az�A��A?}A��A=qA��AG�A
=AA�A�A��A`BA��A�#A
�`A
~�A
A�yA��A��A�7AhsA�A�/AjA$�A�TA��A�AXA��A�A��AdZA%A��A��A �@��@�^5@�{@�`B@��D@�1@�  @��F@�33@��\@�{@�K�@�S�@���@�9X@�=q@�F@旍@� �@�;d@���@���@�h@��u@���@߾w@߶F@�;d@���@�1@�=q@؛�@�33@�V@�x�@�S�@��@҇+@�7L@υ@�"�@��@�=q@ˮ@��@Ǖ�@�;d@�o@Ƈ+@��T@�7L@�z�@��@�\)@�^5@��@�V@�  @���@�%@���@�o@�V@�V@�M�@�bN@�9X@��y@�j@�l�@���@�@���@�7L@���@��j@�(�@�^5@�x�@�%@��`@��@�j@� �@��F@�l�@��@�$�@�?}@� �@���@�dZ@�33@�33@�;d@�@���@���@�-@�hs@��9@��@���@��P@�+@��@���@��!@���@�@�G�@��j@�Q�@�dZ@���@�~�@�n�@�-@���@���@��7@���@�r�@��;@���@��@��y@���@�5?@��T@��h@�p�@��@��j@�r�@�I�@�1'@��;@���@�l�@�"�@���@�$�@���@�p�@�O�@��@�Ĝ@��@�bN@�(�@��;@��w@�l�@���@�Ĝ@�bN@�I�@�b@���@�dZ@�o@��H@�ȴ@���@�n�@�5?@��@�`B@�j@���@�"�@��y@��!@���@�n�@�=q@�$�@�{@���@���@�%@�1'@��m@��F@���@��P@�l�@�33@�+@�o@��@��!@�V@��@���@��h@��`@��@�w@~{@|��@|9X@{33@yG�@xA�@v�+@v@u�T@u��@u��@u@u�-@u`B@tj@t�@sƨ@s@rn�@q�@q�^@q��@qG�@pr�@pA�@pb@o��@ol�@o;d@n�@nE�@m@m�h@m�@l�@l9X@k�
@k�F@k�F@k��@k"�@j��@jn�@j�@i�@i��@ihs@iX@iG�@i7L@h��@h��@h��@g�@gK�@g
=@f�+@e�@e@e�-@e�-@e�h@d��@d(�@d�@d1@c��@co@b~�@a�@a��@aX@`��@`�@`1'@_��@_�@_|�@_\)@_+@^�@^��@^��@^�+@^{@]�@\�/@\�@[��@[dZ@["�@Z�@Z��@Z�@Y�7@YG�@X�`@W�@WK�@Vȴ@V5?@U@U`B@U�@T�@T�D@Tj@TI�@S�m@S�F@S�@R��@R^5@Q�^@PQ�@O�@O|�@Ol�@OK�@N�y@N�@Nȴ@N��@Nff@N5?@N$�@M��@M/@MV@L�j@K��@K@J�!@J�\@J=q@I��@IG�@I�@H�u@G�;@G|�@F�y@F5?@E�@E��@E?}@E�@D��@D1@CC�@Co@B�!@B-@A��@Ahs@AX@AG�@A&�@A%@@�`@@Ĝ@@�@?��@>��@>�@>�R@>�R@>��@>��@>v�@>v�@>5?@=�-@=`B@=/@<��@<�@<��@<z�@<(�@;�
@;dZ@;S�@;o@:n�@:J@9��@9�#@9�#@9�#@9��@9X@9�@8��@8Ĝ@8�u@8b@7��@7|�@7|�@7l�@7+@7
=@6�@6�R@6�+@6v�@6ff@6E�@6E�@6@5��@5V@4�@4I�@3ƨ@3�@3dZ@333@3o@2�H@2�!@2n�@2M�@2J@2J@1�@1�^@1�^@1��@1��@1x�@1X@17L@0��@0�9@0bN@0 �@/�@/�@/l�@.��@.�y@.��@.��@.v�@.v�@.v�@.ff@.$�@.{@.{@.@-��@-�-@-��@-�h@-`B@,�@,�D@,�D@,9X@+ƨ@+C�@*��@*�\@*M�@*=q@*�@)��@)�^@)x�@)7L@(Ĝ@(�u@(�@(bN@(1'@(1'@(1'@(1'@( �@(  @'�@'l�@';d@&��@&ȴ@&ȴ@&��@&�+@&V@&5?@&{@%�@%@%�-@%�@$��@$�/@$��@$I�@#�m@#ƨ@#�F@#�@#C�@#"�@"��@"��@"�\@"^5@"=q@"J@!�@!�^@!X@!&�@ Ĝ@ �@ Q�@ b@�;@�P@;d@��@�R@��@��@V@�@�@/@��@�@��@j@�
@dZ@o@@��@M�@J@��@��@hs@��@��@��@�u@�@1'@�;@��@�@;d@�R@��@��@ff@{@�T@�T@@�h@�@�@`B@`B@`B@O�@�@V@�@z�@�@�m@�
@ƨ@S�@~�@M�@J@��@�@�#@��@��@hs@X@&�@�9@r�@bN@Q�@Q�@A�@1'@  @��@�w@��@K�@�y@��@ff@{@@�@@p�@O�@?}@/@V@�@�/@�@��@�D@Z@(�@��@ƨ@��@t�@"�@
��@
��@
n�@
=q@	�@	��@	hs@	x�@	G�@	�@	�@��@�`@��@��@�`@Ĝ@�@bN@bN@bN@bN@Q�@ �@��@�@l�@�@
=@�y@�y@�y@�@ȴ@��@E�@$�@{@{@@�@��@�T@�T@�-@p�@O�@/@/@V@��@�@��@�j@��@z�@(�@ƨ@�F@t�@�@��@~�@^5@=q@�@J@��@��@�@��@��@x�@G�@7L@&�@ �`@ ��@ Ĝ@ ��@ ��@ �@ bN@ Q�@ Q�@ Q�@ Q�@ Q�@ A�@ 1'@  �?��;?��w?���?���?�v�?�5??�{?��?��?��-?�V?�V?��?��?���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�7B�7B�1B�7B�7B�7B�7B�7B�DB�DB�=B�=B�=B�JB�\B�hB�{B��B��B�HB�B�yB�`B�BB�B�fBǮB1BB��B��B~�B��B�XB��B��B�Bl�BS�B=qB{BDB+BB��B��B  B1BB��B�B�fB�#B�B�;B�BB�ZB�B��B�B��B�B�5B�;B��B��B�qB�B��B~�Bo�B_;BR�BG�B33B-B(�B�B
��B
��B
��B
�B
�TB
�B
��B
��B
�jB
�!B
��B
��B
�JB
x�B
e`B
[#B
S�B
J�B
[#B
bNB
ZB
VB
O�B
L�B
H�B
>wB
6FB
1'B
(�B
 �B
�B
VB
oB
hB
VB
	7B
  B	��B	��B	��B	��B	�B	�B	�B	�yB	�BB	�/B	��B	ƨB	�qB	�RB	�LB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�bB	�1B	�B	z�B	x�B	t�B	k�B	m�B	k�B	hsB	bNB	aHB	^5B	[#B	T�B	L�B	G�B	C�B	=qB	>wB	:^B	49B	33B	2-B	/B	'�B	�B	�B	�B	oB	\B	PB	
=B	B��B��B��B��B�B�B�B�B�mB�fB�5B�)B�B��B��B�B�
B�B��B��B��B��BǮB�XB�jB�qB�FB�3B�B��B��B��B��B��B��B��B�uB�7B�B�B{�B{�B� B� B}�Bx�Bo�BgmBhsBhsBffBgmBe`BcTBaHB]/BP�BT�BXBXBW
BR�BM�BK�BM�BK�BF�BD�BJ�BK�BJ�BH�BH�BF�BF�BF�BF�BE�BD�BB�B@�BB�BB�BA�B@�B;dB7LB:^B>wB@�B>wB=qB>wB@�B>wB<jB;dB7LB.B&�B$�B)�B-B)�B-B(�B/B-B1'B0!B-B.B1'B0!B.B+B'�B'�B)�B+B-B.B)�B33B33B0!B/B5?B49B/B'�B)�B33B8RB9XB7LB6FB7LB8RB:^B8RB:^B>wB<jB<jB;dBE�BG�BD�BG�BE�BA�BI�BVBQ�BN�B[#B_;BbNBe`Be`BgmBffBe`BbNBl�Bq�Bs�Bt�Bu�Bv�Bv�Bw�Bv�Bv�By�B|�B�B�+B�1B�=B�7B�1B�1B�7B�7B�7B�JB�bB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�3B�3B�9B�?B�LB�XB�XB�dB�qB�}B��B��BÖBŢBŢBǮBɺB��B��B��B��B��B��B�B�B�B�#B�B�
B�HB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	B	
=B	JB	VB	VB	\B	bB	bB	bB	\B	VB	oB	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	#�B	#�B	&�B	'�B	(�B	(�B	-B	.B	33B	9XB	?}B	>wB	>wB	F�B	J�B	S�B	W
B	W
B	W
B	W
B	W
B	VB	VB	ZB	[#B	[#B	^5B	`BB	bNB	cTB	cTB	dZB	hsB	iyB	iyB	jB	l�B	l�B	m�B	p�B	r�B	r�B	t�B	w�B	x�B	z�B	z�B	z�B	y�B	{�B	}�B	~�B	~�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�1B	�7B	�=B	�JB	�PB	�PB	�PB	�PB	�bB	�oB	�oB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�3B	�?B	�LB	�XB	�^B	�jB	�qB	�wB	�}B	��B	��B	ĜB	ŢB	ŢB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�/B	�BB	�HB	�HB	�HB	�NB	�TB	�TB	�ZB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
+B
+B
+B
1B
	7B
	7B
	7B

=B
DB
JB
PB
PB
PB
VB
\B
\B
\B
\B
bB
bB
bB
\B
\B
bB
hB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
 �B
 �B
 �B
!�B
"�B
!�B
!�B
"�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
-B
,B
-B
-B
-B
.B
.B
.B
/B
.B
.B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
2-B
33B
2-B
33B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
8RB
8RB
8RB
9XB
:^B
:^B
;dB
:^B
:^B
:^B
;dB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
?}B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
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
R�B
S�B
S�B
T�B
S�B
T�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
W
B
W
B
XB
XB
XB
W
B
XB
YB
YB
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
[#B
[#B
[#B
\)B
\)B
\)B
]/B
]/B
^5B
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
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
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
gmB
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
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
l�B
l�B
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
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�7B�7B�1B�7B�7B�7B�7B�7B�DB�DB�=B�=B�=B�dB��B�B�mB��B�jB�B��B�KB�mB�tB�?B��B�}B
#B?B�6B�"B�<B��B�VB�HB��B��BqvBX�BC�B�B�B�B3B��B�LB �B	RB�B��B�aB�mB��B�YBߊB��B��B��B��B��B��B�B�B�B�B��B�OB��B��B�ABq�Ba-BT�BI�B6+B.�B*KBB�B
�}B
��B
�TB
��B
��B
�B
�uB
��B
�GB
��B
��B
��B
{�B
hXB
\xB
U�B
K�B
[=B
c:B
[qB
W
B
P�B
M�B
I�B
@OB
7�B
2GB
*eB
"�B
�B
}B
�B
�B
�B

=B
�B	�B	�^B	�*B	�8B	�B	�B	�B	�0B	�B	�5B	өB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�nB	�nB	�-B	�OB	��B	��B	�#B	�aB	|�B	y�B	vB	m)B	m�B	lB	iDB	cTB	b4B	_B	\)B	VmB	N�B	H�B	D�B	>�B	>�B	;JB	5�B	3�B	2|B	/�B	)_B	 �B	�B	�B	B	�B	"B	B	SB��B��B��B��B�B�vB�oB�qB�B�8B��B�/B�QB�SB��B�SB�$B�B�2B�FB�hB͟B�B�B�qB�(B��B�9B��B��B��B��B�/B�yB�KB�YB��B�^B��B�MB~B}"B�iB�iB~wBy�Bq�Bi�Bi�Bi�Bg�Bh$Bf2Bd&Bb4B^�BS�BVBX�BX�BW�BS�BO(BMBN�BL�BH1BE�BKBLBKBI7BIBGEBGEBGBGBFBE9BC{BAoBC-BC-BBABAUB<�B9	B;B>�B@�B?.B>B>�B@�B>�B=B<B88B0UB)�B'�B+�B.�B+�B.B*�B/�B-�B1[B0�B-�B.�B1[B0oB.�B,"B)yB)DB+B,B-�B.�B+QB3�B3�B1'B0;B5�B4�B0!B)�B+�B4B8�B9�B7�B6�B8B9	B:�B9	B;B>�B=VB=qB<�BF%BHKBE�BH�BF�BCaBJ�BVmBS&BP}B[�B_�Bb�Be�Be�Bg�Bf�BfBc�BmCBrBtBuBvBwBw2BxBw2Bw�Bz�B}�B�aB�zB�fB�XB�RB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�+B�KB�B�BB�vB�`B�>B�B�_B�_B�0B�KB��B�=B�kB�iB��B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�#B�JB�B� B�:B�@B�2B�EB�_B�eB�WBٚB�+B�B��B�B��B��B��B��B�B��B�B�B�B�B�RB��B��B	mB	
rB	~B	pB	�B	�B	�B	}B	�B	�B	B	B	�B	�B	�B	�B	�B	�B	�B	 �B	!B	$B	$@B	'B	(>B	)DB	)yB	-�B	.�B	3�B	9�B	?�B	>�B	?HB	GB	KxB	T,B	W?B	W?B	W$B	W?B	W$B	VSB	VSB	ZQB	[qB	[qB	^jB	`�B	b�B	cnB	c�B	d�B	h�B	i�B	i�B	j�B	l�B	l�B	m�B	p�B	r�B	r�B	uB	xB	x�B	z�B	z�B	z�B	zB	|B	~B	.B	B	�4B	�;B	�'B	�'B	�AB	�AB	�-B	�GB	�uB	�zB	�fB	��B	�rB	�~B	�jB	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	�&B	�B	�ZB	�8B	�0B	�6B	�"B	�"B	�=B	�CB	�CB	�OB	�UB	��B	�hB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ĶB	żB	��B	��B	�	B	�=B	�B	�B	�&B	�B	�,B	�B	�9B	�B	�?B	�+B	�EB	�EB	�EB	�7B	�QB	ڠB	�dB	�\B	�|B	�B	�|B	�B	�nB	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�$B	�B	�B	�B	�B	�B	�"B	�B	�B	�"B	�B
 B
;B
'B
AB
;B
'B
AB
aB
3B
MB
GB
9B
EB
EB
+B
EB
EB
zB
KB
	lB
	lB
	RB

rB
^B
~B
PB
jB
jB
pB
vB
vB
vB
vB
}B
}B
bB
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
 �B
 �B
!B
!�B
"�B
!�B
!�B
#B
$B
$�B
%B
%�B
%�B
&B
&B
'B
'B
'B
)*B
)*B
)B
*B
)�B
*B
)�B
*B
*B
*0B
*B
+B
+B
,=B
-B
,"B
-)B
-)B
-)B
./B
./B
.IB
/5B
./B
.IB
0;B
0;B
0;B
1AB
2GB
2GB
2aB
2GB
3MB
2GB
3MB
4nB
4nB
4nB
4nB
5ZB
5tB
5�B
6`B
6zB
7fB
7fB
8�B
8lB
8lB
9rB
:xB
:xB
;B
:xB
:�B
:�B
;�B
<�B
=�B
=�B
=�B
=�B
=�B
>�B
?�B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
LB
OB
OB
O�B
O�B
O�B
PB
Q B
Q B
Q B
QB
Q4B
SB
S�B
S�B
T�B
TB
UB
TB
UB
UB
UB
UB
U2B
VB
W$B
W$B
X+B
X+B
XEB
W?B
X+B
YB
Y1B
Y1B
Y1B
Y1B
ZB
Z7B
Z7B
Z7B
Z7B
Z7B
[WB
[WB
[=B
[=B
[=B
\]B
\]B
\]B
]IB
]dB
^OB
_VB
_VB
_VB
_VB
_pB
_;B
_VB
`BB
`\B
`\B
`\B
`\B
aHB
aHB
abB
a|B
abB
abB
bhB
b�B
bhB
cnB
cnB
dZB
dZB
dZB
dtB
c�B
cnB
dtB
dZB
dtB
dtB
dZB
d�B
e`B
e`B
ezB
ezB
f�B
g�B
gmB
g�B
gmB
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
jB
jB
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
l�B
l�B
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
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<I��<t!<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201709170034372017091700343720170917003437201806221230412018062212304120180622123041201804050425512018040504255120180405042551  JA  ARFMdecpA19c                                                                20170913093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170913003509  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170913003511  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170913003511  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170913003512  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170913003512  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170913003512  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170913003512  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170913003513  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170913003513                      G�O�G�O�G�O�                JA  ARUP                                                                        20170913005632                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170913153223  CV  JULD            G�O�G�O�F�-�                JM  ARCAJMQC2.0                                                                 20170916153437  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170916153437  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404192551  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033041  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121516                      G�O�G�O�G�O�                