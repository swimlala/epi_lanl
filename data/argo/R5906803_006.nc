CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-05-16T17:22:41Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230516172241  20230516172241  5906803 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            NB_SAMPLE_CTD      A   AO  9276                            2B  A   NAVIS_A                         1437                            170425                          863 @���O�`1   @�⡐g�@:�L�_��d�3��1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         A   A   A       @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @7
>@��@��A�\A"�\AB�\Ab�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�C (�C(�C(�C(�C(�C
(�C(�C(�C(�C(�C(�C(�C(�C(�C(�C(�C (�C"(�C$(�C&(�C((�C*(�C,(�C.(�C0(�C2(�C4(�C6(�C8(�C:(�C<(�C>(�C@(�CB(�CD(�CF(�CH(�CJ(�CL(�CN(�CP(�CR(�CT(�CV(�CX(�CZ(�C\(�C^(�C`(�Cb(�Cd(�Cf(�Ch(�Cj(�Cl(�Cn(�Cp(�Cr(�Ct(�Cv(�Cx(�Cz(�C|(�C~(�C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{D 
=D �=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D	
=D	�=D

=D
�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D 
=D �=D!
=D!�=D"
=D"�=D#
=D#�=D$
=D$�=D%
=D%�=D&
=D&�=D'
=D'�=D(
=D(�=D)
=D)�=D*
=D*�=D+
=D+�=D,
=D,�=D-
=D-�=D.
=D.�=D/
=D/�=D0
=D0�=D1
=D1�=D2
=D2�=D3
=D3�=D4
=D4�=D5
=D5�=D6
=D6�=D7
=D7�=D8
=D8�=D9
=D9�=D:
=D:�=D;
=D;�=D<
=D<�=D=
=D=�=D>
=D>�=D?
=D?�=D@
=D@�=DA
=DA�=DB
=DB�=DC
=DC�=DD
=DD�=DE
=DE�=DF
=DF�=DG
=DG�=DH
=DH�=DI
=DI�=DJ
=DJ�=DK
=DK�=DL
=DL�=DM
=DM�=DN
=DN�=DO
=DO�=DP
=DP�=DQ
=DQ�=DR
=DR�=DS
=DS�=DT
=DT�=DU
=DU�=DV
=DV�=DW
=DW�=DX
=DX�=DY
=DY�=DZ
=DZ�=D[
=D[�=D\
=D\�=D]
=D]�=D^
=D^�=D_
=D_�=D`
=D`�=Da
=Da�=Db
=Db�=Dc
=Dc�=Dd
=Dd�=De
=De�=Df
=Df�=Dg
=Dg�=Dh
=Dh�=Di
=Di�=Dj
=Dj�=Dk
=Dk�=Dl
=Dl�=Dm
=Dm�=Dn
=Dn�=Do
=Do�=Dp
=Dp�=Dq
=Dq�=Dr
=Dr�=Ds
=Ds�=Dt
=Dt�=Du
=Du�=Dv
=Dv�=Dw
=Dw�=Dx
=Dx�=Dy
=Dy�=Dz
=Dz�=D{
=D{�=D|
=D|�=D}
=D}�=D~
=D~�=D
=D�=D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�EDD��D�D�EDÅD��D�D�EDąD��D�D�EDŅD��D�D�EDƅD��D�D�EDǅD��D�D�EDȅD��D�D�EDɅD��D�D�EDʅD��D�D�ED˅D��D�D�ED̅D��D�D�EDͅD��D�D�ED΅D��D�D�EDυD��D�D�EDЅD��D�D�EDхD��D�D�ED҅D��D�D�EDӅD��D�D�EDԅD��D�D�EDՅD��D�D�EDօD��D�D�EDׅD��D�D�ED؅D��D�D�EDمD��D�D�EDڅD��D�D�EDۅD��D�D�ED܅D��D�D�ED݅D��D�D�EDޅD��D�D�ED߅D��D�D�ED��D��D�D�ED�D��D�D�ED�D��D�D�ED�D��D�D�ED�D��D�D�ED�D��D�D�ED�D��D�D�ED�D��D�D�ED�D��D�D�ED�D��D�D�ED�D��D�D�ED�D���D�D�ED�D��D�D�ED�D��D�D�ED�D��D�D�ED�D��D�D�ED��D��D�D�ED�D��D�D�ED�D��D�D�ED�D��D�D�ED�D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��D��D�D�ED��RD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�(�A�(�A�+A�/A�5?A�7LA�7LA�9XA�9XA�9XA�=qA�=qA�;dA�?}A�A�A�A�A�A�A�C�A�C�A�5?AЙ�A��TA�{A�A�A�VA�VA���A���A���A�;dA�|�A��A�dZA�~�A��A��\A�A���A��A�A�A�x�A��A��-A��DA��7A���A�ffA���A�?}A��A�5?A�-A� �A��#A�XA�ƨA�A�A��A�ĜA��!A�ZA� �A���A���A��A�|�A���A�XA�oA��`A��A��A��#A�I�A��TA�
=A��9A�A�A�|�A���A�C�A�A��TA�Q�A��mA��7A�O�A�ȴA���A�5?A�?}A��A��A�ƨA��^A�~�A��A�O�A���A�bA���A��A���A��RA�XA�-A���A���A�ĜA���A��^A�A�ZA��;A���A�A�A�JA�l�A�%A���A���A��DA���A��^A��jA��uA�VA� �A�ĜA�l�A���A�mA}��A|��A|=qA{G�Ay"�Av�\As�mAr~�Ar9XAp��Ak�^Ai�Ae��Acx�Ab~�AbbAaVA_�7A^�A]�A]\)A\�jA[��A[O�A[/AZffAX�`AW�7AV �AT�AR��AQ�hAO�;AN��AM�
AL�AJr�AE`BAB��A?�mA=��A;A:�uA:Q�A:�A9�FA9�A9`BA8��A81'A8A7�wA7hsA6�A4�`A2�uA0bA.{A-K�A,��A,1'A+�A+K�A)��A(��A';dA%�#A%;dA$n�A#�PA"{A �yA =qA�mA��AI�AA�A�A��Ap�A��AM�A�A��Ax�AM�AS�A�DA
=A�wA;dAVA��A��A5?AƨA7LA%Az�A1'AA�
A
�!A
�AM�A^5A��Ap�A�A�A�#A%Az�A��@�33@�J@��h@�ff@�9X@�S�@�o@�E�@�X@��`@�Ĝ@��j@��@�bN@�|�@��y@�@�j@��@�V@� �@�{@�ƨ@�G�@�n�@��@��@�+@�M�@�O�@�j@�C�@�;d@�o@�ȴ@�/@��@�?}@�V@ӶF@��#@�I�@���@�Z@��@�ff@�$�@��@���@ə�@�p�@�V@ȼj@�bN@�1@ǝ�@��@��@Ł@�7L@��@Ĭ@��y@�G�@��u@�I�@���@�t�@�;d@��+@��@���@���@�I�@��;@��R@�J@��-@�hs@�?}@�&�@��@�V@��/@�r�@���@���@�dZ@��^@��@�K�@�~�@�=q@��@��@�I�@���@�
=@�=q@�X@���@���@���@�G�@�Ĝ@��@�1'@�ƨ@�33@�"�@�
=@��@���@�^5@���@��@��D@�9X@�1@�S�@�^5@��#@�hs@�&�@��@��@���@�1@��P@�"�@�$�@���@�p�@�G�@��@�l�@�
=@��H@���@�V@���@�`B@���@�A�@�b@��
@��@��P@���@�33@�ȴ@��+@�=q@��@��#@���@���@�x�@�O�@�%@��@�1'@�1@���@��y@���@�n�@�5?@���@�@�x�@�%@���@���@�r�@�Q�@� �@�b@��;@�@�E�@���@�@�@��-@��@��@�%@���@�j@�A�@�1'@�  @���@��y@��!@�v�@��@��^@��@�G�@�7L@�&�@�V@�Ĝ@�z�@�Z@�1'@��@��P@�dZ@�+@�o@�
=@��H@�ȴ@��+@�^5@�M�@�=q@�J@���@��@���@���@��@�p�@�%@���@���@�j@�@K�@
=@~�R@~@}�@}�T@}@}��@}�@}`B@}/@|��@|�j@|��@|��@|j@|I�@|9X@|9X@|(�@{�
@{C�@{o@z��@zn�@zJ@y�7@y7L@x��@x�9@x�@w�w@wl�@vȴ@v��@vv�@vff@u�@uV@t��@t�@tI�@s�@s33@s"�@r�\@qx�@p�@p  @oK�@nȴ@n�R@n��@n5?@m�@m�@m�@l�@lj@k�m@kt�@k33@j�@j^5@i��@i�#@i��@i�@i%@h�u@hQ�@hA�@h  @g��@gl�@g\)@f��@fȴ@f��@f$�@e@e�@d�/@d�@d�@c�F@ct�@c"�@b��@b-@a��@`�`@`A�@` �@_��@_�P@^�R@]�@]p�@]`B@]?}@]�@]V@\�@\�@[��@[dZ@[o@Z�@Z��@Z~�@Z=q@Y�@Yx�@Y%@Y%@X��@X��@XQ�@W�;@W��@Vȴ@Vv�@Vff@VE�@V$�@U�@U@U��@T�@T�@Tz�@T(�@Sƨ@SS�@SC�@SC�@SC�@S33@S33@S33@S"�@S@Q��@Qhs@P�`@P�@P �@O�w@N�@NV@M�-@M�h@M�h@M�h@M�h@M?}@L�j@LI�@K��@K��@K��@KdZ@J��@J�\@Jn�@J-@I��@Ihs@IX@I&�@HbN@G�@G�@G��@G��@G��@G�@G
=@F�+@Fv�@FE�@F$�@F@E��@Ep�@D�@Dz�@D1@C�F@C��@C��@C�F@Cƨ@C�
@C�
@C��@C��@CdZ@CC�@CC�@C33@B��@B�\@B=q@B-@A�@Ahs@AG�@A%@@�`@@A�@@b@@b@?��@?�P@?�@>�y@>�R@>v�@>V@>V@=��@=p�@=�@<�/@<z�@<Z@<9X@;ƨ@;dZ@;C�@;"�@:��@:��@:M�@:J@9�^@9hs@9G�@9&�@8�`@8�u@8bN@8b@7�;@7��@7��@7�w@7�@7|�@6�y@6v�@65?@6{@5�@4��@4z�@4z�@4j@4(�@3�m@3�@3o@2��@2�\@2^5@2M�@1��@1G�@1%@0�`@0�9@0�@0bN@0Q�@0 �@/��@/l�@/�@.�y@.�@.�@.ȴ@.�R@.{@-��@-��@-��@-p�@-/@-V@,�@,�@,��@,�@,�D@,I�@,�@+��@+��@+��@+��@+��@+�F@+t�@+33@*�@*��@*�!@*�\@*^5@*=q@*�@)��@)�^@)X@)%@(Ĝ@(�u@(�@(r�@(Q�@(A�@(b@'�P@'
=@&�@&�@&��@&V@&$�@%�@%��@%��@%`B@%�@$�@$�D@$�@#�m@#�F@#�F@#��@"��@"��@"��@"�\@"n�@"-@!x�@!%@ �9@ r�@�w@�P@|�@;d@+@�@��@ȴ@��@v�@$�@��@�@�/@��@j@I�@1@ƨ@��@�@S�@"�@��@�\@~�@n�@=q@-@��@�#@�7@�7@x�@X@�@��@�`@Ĝ@�9@Q�@b@  @�@�@�R@v�@5?@@@@�@�h@O�@/@/@/@�@V@V@��@�/@I�@��@��@t�@33@o@�@��@�!@~�@�@��@��@��@hs@G�@7L@��@�9@�u@Q�@  @��@�w@�w@�@�@�@��@;d@�y@�@ȴ@�R@��@��@��@ff@V@$�@@?}@�@�@�@V@�/@�j@�@�@��@�D@z�@z�@z�@z�@�D@z�@j@Z@I�@9X@�F@33@
�H@
�\@
n�@
M�@
-@
J@	�#@	�#@	�^@	�^@	��@	x�@��@�`@�`@�9@�@ �@�@�P@|�@\)@;d@
=@�@ȴ@ȴ@ȴ@ȴ@�R@�R@��@��@��@��@�+@v�@ff@�T@p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�(�A�(�A�+A�/A�5?A�7LA�7LA�9XA�9XA�9XA�=qA�=qA�;dA�?}A�A�A�A�A�A�A�C�A�C�A�5?AЙ�A��TA�{A�A�A�VA�VA���A���A���A�;dA�|�A��A�dZA�~�A��A��\A�A���A��A�A�A�x�A��A��-A��DA��7A���A�ffA���A�?}A��A�5?A�-A� �A��#A�XA�ƨA�A�A��A�ĜA��!A�ZA� �A���A���A��A�|�A���A�XA�oA��`A��A��A��#A�I�A��TA�
=A��9A�A�A�|�A���A�C�A�A��TA�Q�A��mA��7A�O�A�ȴA���A�5?A�?}A��A��A�ƨA��^A�~�A��A�O�A���A�bA���A��A���A��RA�XA�-A���A���A�ĜA���A��^A�A�ZA��;A���A�A�A�JA�l�A�%A���A���A��DA���A��^A��jA��uA�VA� �A�ĜA�l�A���A�mA}��A|��A|=qA{G�Ay"�Av�\As�mAr~�Ar9XAp��Ak�^Ai�Ae��Acx�Ab~�AbbAaVA_�7A^�A]�A]\)A\�jA[��A[O�A[/AZffAX�`AW�7AV �AT�AR��AQ�hAO�;AN��AM�
AL�AJr�AE`BAB��A?�mA=��A;A:�uA:Q�A:�A9�FA9�A9`BA8��A81'A8A7�wA7hsA6�A4�`A2�uA0bA.{A-K�A,��A,1'A+�A+K�A)��A(��A';dA%�#A%;dA$n�A#�PA"{A �yA =qA�mA��AI�AA�A�A��Ap�A��AM�A�A��Ax�AM�AS�A�DA
=A�wA;dAVA��A��A5?AƨA7LA%Az�A1'AA�
A
�!A
�AM�A^5A��Ap�A�A�A�#A%Az�A��@�33@�J@��h@�ff@�9X@�S�@�o@�E�@�X@��`@�Ĝ@��j@��@�bN@�|�@��y@�@�j@��@�V@� �@�{@�ƨ@�G�@�n�@��@��@�+@�M�@�O�@�j@�C�@�;d@�o@�ȴ@�/@��@�?}@�V@ӶF@��#@�I�@���@�Z@��@�ff@�$�@��@���@ə�@�p�@�V@ȼj@�bN@�1@ǝ�@��@��@Ł@�7L@��@Ĭ@��y@�G�@��u@�I�@���@�t�@�;d@��+@��@���@���@�I�@��;@��R@�J@��-@�hs@�?}@�&�@��@�V@��/@�r�@���@���@�dZ@��^@��@�K�@�~�@�=q@��@��@�I�@���@�
=@�=q@�X@���@���@���@�G�@�Ĝ@��@�1'@�ƨ@�33@�"�@�
=@��@���@�^5@���@��@��D@�9X@�1@�S�@�^5@��#@�hs@�&�@��@��@���@�1@��P@�"�@�$�@���@�p�@�G�@��@�l�@�
=@��H@���@�V@���@�`B@���@�A�@�b@��
@��@��P@���@�33@�ȴ@��+@�=q@��@��#@���@���@�x�@�O�@�%@��@�1'@�1@���@��y@���@�n�@�5?@���@�@�x�@�%@���@���@�r�@�Q�@� �@�b@��;@�@�E�@���@�@�@��-@��@��@�%@���@�j@�A�@�1'@�  @���@��y@��!@�v�@��@��^@��@�G�@�7L@�&�@�V@�Ĝ@�z�@�Z@�1'@��@��P@�dZ@�+@�o@�
=@��H@�ȴ@��+@�^5@�M�@�=q@�J@���@��@���@���@��@�p�@�%@���@���@�j@�@K�@
=@~�R@~@}�@}�T@}@}��@}�@}`B@}/@|��@|�j@|��@|��@|j@|I�@|9X@|9X@|(�@{�
@{C�@{o@z��@zn�@zJ@y�7@y7L@x��@x�9@x�@w�w@wl�@vȴ@v��@vv�@vff@u�@uV@t��@t�@tI�@s�@s33@s"�@r�\@qx�@p�@p  @oK�@nȴ@n�R@n��@n5?@m�@m�@m�@l�@lj@k�m@kt�@k33@j�@j^5@i��@i�#@i��@i�@i%@h�u@hQ�@hA�@h  @g��@gl�@g\)@f��@fȴ@f��@f$�@e@e�@d�/@d�@d�@c�F@ct�@c"�@b��@b-@a��@`�`@`A�@` �@_��@_�P@^�R@]�@]p�@]`B@]?}@]�@]V@\�@\�@[��@[dZ@[o@Z�@Z��@Z~�@Z=q@Y�@Yx�@Y%@Y%@X��@X��@XQ�@W�;@W��@Vȴ@Vv�@Vff@VE�@V$�@U�@U@U��@T�@T�@Tz�@T(�@Sƨ@SS�@SC�@SC�@SC�@S33@S33@S33@S"�@S@Q��@Qhs@P�`@P�@P �@O�w@N�@NV@M�-@M�h@M�h@M�h@M�h@M?}@L�j@LI�@K��@K��@K��@KdZ@J��@J�\@Jn�@J-@I��@Ihs@IX@I&�@HbN@G�@G�@G��@G��@G��@G�@G
=@F�+@Fv�@FE�@F$�@F@E��@Ep�@D�@Dz�@D1@C�F@C��@C��@C�F@Cƨ@C�
@C�
@C��@C��@CdZ@CC�@CC�@C33@B��@B�\@B=q@B-@A�@Ahs@AG�@A%@@�`@@A�@@b@@b@?��@?�P@?�@>�y@>�R@>v�@>V@>V@=��@=p�@=�@<�/@<z�@<Z@<9X@;ƨ@;dZ@;C�@;"�@:��@:��@:M�@:J@9�^@9hs@9G�@9&�@8�`@8�u@8bN@8b@7�;@7��@7��@7�w@7�@7|�@6�y@6v�@65?@6{@5�@4��@4z�@4z�@4j@4(�@3�m@3�@3o@2��@2�\@2^5@2M�@1��@1G�@1%@0�`@0�9@0�@0bN@0Q�@0 �@/��@/l�@/�@.�y@.�@.�@.ȴ@.�R@.{@-��@-��@-��@-p�@-/@-V@,�@,�@,��@,�@,�D@,I�@,�@+��@+��@+��@+��@+��@+�F@+t�@+33@*�@*��@*�!@*�\@*^5@*=q@*�@)��@)�^@)X@)%@(Ĝ@(�u@(�@(r�@(Q�@(A�@(b@'�P@'
=@&�@&�@&��@&V@&$�@%�@%��@%��@%`B@%�@$�@$�D@$�@#�m@#�F@#�F@#��@"��@"��@"��@"�\@"n�@"-@!x�@!%@ �9@ r�@�w@�P@|�@;d@+@�@��@ȴ@��@v�@$�@��@�@�/@��@j@I�@1@ƨ@��@�@S�@"�@��@�\@~�@n�@=q@-@��@�#@�7@�7@x�@X@�@��@�`@Ĝ@�9@Q�@b@  @�@�@�R@v�@5?@@@@�@�h@O�@/@/@/@�@V@V@��@�/@I�@��@��@t�@33@o@�@��@�!@~�@�@��@��@��@hs@G�@7L@��@�9@�u@Q�@  @��@�w@�w@�@�@�@��@;d@�y@�@ȴ@�R@��@��@��@ff@V@$�@@?}@�@�@�@V@�/@�j@�@�@��@�D@z�@z�@z�@z�@�D@z�@j@Z@I�@9X@�F@33@
�H@
�\@
n�@
M�@
-@
J@	�#@	�#@	�^@	�^@	��@	x�@��@�`@�`@�9@�@ �@�@�P@|�@\)@;d@
=@�@ȴ@ȴ@ȴ@ȴ@�R@�R@��@��@��@��@�+@v�@ff@�T@p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B|�Br�B[#B?}B8RB&�B�B�B�B�BoB�B�B�B�B�B�B�B�B�B$�B(�B(�B&�B,B/BH�BK�BM�BO�BN�BM�BM�BM�BM�BM�BO�BN�BL�BK�BJ�BJ�BI�BG�BF�BE�BC�BA�B<jB:^B8RB6FB2-B.B(�B$�B�B�B�BbBJB��B�mB�B��BÖB�qB�LB�-B��B�uBv�B]/BYBS�BR�BL�B>wB(�B�BoB��B�B�BĜB�B��B� Bm�BVBB�B=qB49B&�B �BhB
��B
�fB
�B
��B
��B
��B
�B
��B
x�B
dZB
bNB
_;B
[#B
VB
Q�B
J�B
?}B
33B
(�B
%�B
�B
uB
B	��B	�sB	�`B	�5B	�}B	�!B	��B	�\B	�=B	�+B	�B	z�B	v�B	q�B	m�B	k�B	gmB	dZB	cTB	`BB	\)B	S�B	O�B	H�B	>wB	5?B	-B	$�B	�B	�B	DB�B�B��BÖB�qB�RB�FB�FB�9B�-B�-B�B�B�B��B��B��B��B��B�oB�JB�+B�%B�B�B�B{�By�Bu�Br�Bn�Bm�BjBgmBdZBbNBaHB_;B_;B[#BZBXBQ�BP�BO�BK�BJ�BI�BH�BF�BC�BB�B@�B?}B=qB=qB<jB;dB;dB;dB:^B9XB9XB8RB7LB6FB5?B1'B2-B/B-B,B+B)�B)�B'�B%�B%�B$�B$�B#�B%�B$�B#�B"�B"�B"�B"�B"�B"�B!�B"�B!�B"�B"�B#�B$�B"�B"�B$�B$�B'�B(�B'�B(�B(�B(�B(�B)�B+B)�B)�B(�B+B-B1'B0!B49B5?B7LB<jB=qB?}B@�B@�B@�B@�B@�B@�BA�BA�BB�BB�BB�BC�BE�BF�BF�BF�BE�BJ�BL�BM�BN�BO�BP�BP�BR�BS�BS�BVBW
BW
BYBZB\)B^5B_;B_;B`BB`BB`BBaHBdZBffBgmBiyBjBl�Bm�Bm�Bp�Bp�Bq�Bs�Bu�Bv�Bz�B}�B�B�1B�=B�JB�PB�VB�bB�uB�{B�{B��B��B��B��B��B��B��B��B�B�3B�LB�^B�jB�qB�wBBɺBɺB��BȴBǮBȴB��B��B��B��B��B�B�
B�B�5B�NB�mB�sB�B�B�B�B�B�B�B��B��B��B��B��B��B	B	B	B	%B	%B	1B	JB	VB	bB	oB	uB	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	#�B	)�B	.B	0!B	1'B	1'B	1'B	33B	6FB	7LB	;dB	=qB	?}B	?}B	@�B	B�B	H�B	J�B	L�B	O�B	Q�B	S�B	T�B	VB	VB	W
B	YB	\)B	\)B	]/B	bNB	cTB	dZB	ffB	gmB	iyB	k�B	k�B	l�B	m�B	m�B	n�B	o�B	o�B	p�B	q�B	r�B	s�B	s�B	v�B	x�B	z�B	{�B	� B	�B	�B	�+B	�DB	�DB	�DB	�JB	�JB	�PB	�PB	�VB	�\B	�bB	�bB	�bB	�hB	�oB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�'B	�3B	�9B	�?B	�LB	�dB	�wB	��B	ÖB	ŢB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�5B	�5B	�;B	�BB	�BB	�HB	�NB	�TB	�ZB	�`B	�`B	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B
	7B
	7B

=B

=B

=B

=B
DB
DB
DB
JB
JB
JB
JB
JB
PB
PB
VB
\B
\B
bB
hB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
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
-B
-B
.B
.B
.B
/B
/B
0!B
/B
0!B
1'B
1'B
2-B
2-B
2-B
33B
33B
33B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
8RB
8RB
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
:^B
:^B
:^B
;dB
<jB
<jB
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
@�B
@�B
@�B
@�B
A�B
@�B
@�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
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
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
M�B
N�B
O�B
O�B
N�B
N�B
N�B
O�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
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
\)B
\)B
\)B
\)B
]/B
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
`BB
`BB
`BB
`BB
`BB
`BB
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
ffB
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
k�B
k�B
k�B
k�B
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
p�B
p�B
p�B
p�B
p�B
q�B
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
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B|�Br�B[#B?}B8RB&�B�B�B�B�BoB�B�B�B�B�B�B�B�B�B$�B(�B(�B&�B,B/BH�BK�BM�BO�BN�BM�BM�BM�BM�BM�BO�BN�BL�BK�BJ�BJ�BI�BG�BF�BE�BC�BA�B<jB:^B8RB6FB2-B.B(�B$�B�B�B�BbBJB��B�mB�B��BÖB�qB�LB�-B��B�uBv�B]/BYBS�BR�BL�B>wB(�B�BoB��B�B�BĜB�B��B� Bm�BVBB�B=qB49B&�B �BhB
��B
�fB
�B
��B
��B
��B
�B
��B
x�B
dZB
bNB
_;B
[#B
VB
Q�B
J�B
?}B
33B
(�B
%�B
�B
uB
B	��B	�sB	�`B	�5B	�}B	�!B	��B	�\B	�=B	�+B	�B	z�B	v�B	q�B	m�B	k�B	gmB	dZB	cTB	`BB	\)B	S�B	O�B	H�B	>wB	5?B	-B	$�B	�B	�B	DB�B�B��BÖB�qB�RB�FB�FB�9B�-B�-B�B�B�B��B��B��B��B��B�oB�JB�+B�%B�B�B�B{�By�Bu�Br�Bn�Bm�BjBgmBdZBbNBaHB_;B_;B[#BZBXBQ�BP�BO�BK�BJ�BI�BH�BF�BC�BB�B@�B?}B=qB=qB<jB;dB;dB;dB:^B9XB9XB8RB7LB6FB5?B1'B2-B/B-B,B+B)�B)�B'�B%�B%�B$�B$�B#�B%�B$�B#�B"�B"�B"�B"�B"�B"�B!�B"�B!�B"�B"�B#�B$�B"�B"�B$�B$�B'�B(�B'�B(�B(�B(�B(�B)�B+B)�B)�B(�B+B-B1'B0!B49B5?B7LB<jB=qB?}B@�B@�B@�B@�B@�B@�BA�BA�BB�BB�BB�BC�BE�BF�BF�BF�BE�BJ�BL�BM�BN�BO�BP�BP�BR�BS�BS�BVBW
BW
BYBZB\)B^5B_;B_;B`BB`BB`BBaHBdZBffBgmBiyBjBl�Bm�Bm�Bp�Bp�Bq�Bs�Bu�Bv�Bz�B}�B�B�1B�=B�JB�PB�VB�bB�uB�{B�{B��B��B��B��B��B��B��B��B�B�3B�LB�^B�jB�qB�wBBɺBɺB��BȴBǮBȴB��B��B��B��B��B�B�
B�B�5B�NB�mB�sB�B�B�B�B�B�B�B��B��B��B��B��B��B	B	B	B	%B	%B	1B	JB	VB	bB	oB	uB	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	#�B	)�B	.B	0!B	1'B	1'B	1'B	33B	6FB	7LB	;dB	=qB	?}B	?}B	@�B	B�B	H�B	J�B	L�B	O�B	Q�B	S�B	T�B	VB	VB	W
B	YB	\)B	\)B	]/B	bNB	cTB	dZB	ffB	gmB	iyB	k�B	k�B	l�B	m�B	m�B	n�B	o�B	o�B	p�B	q�B	r�B	s�B	s�B	v�B	x�B	z�B	{�B	� B	�B	�B	�+B	�DB	�DB	�DB	�JB	�JB	�PB	�PB	�VB	�\B	�bB	�bB	�bB	�hB	�oB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�'B	�3B	�9B	�?B	�LB	�dB	�wB	��B	ÖB	ŢB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�5B	�5B	�;B	�BB	�BB	�HB	�NB	�TB	�ZB	�`B	�`B	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B
	7B
	7B

=B

=B

=B

=B
DB
DB
DB
JB
JB
JB
JB
JB
PB
PB
VB
\B
\B
bB
hB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
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
-B
-B
.B
.B
.B
/B
/B
0!B
/B
0!B
1'B
1'B
2-B
2-B
2-B
33B
33B
33B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
8RB
8RB
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
:^B
:^B
:^B
;dB
<jB
<jB
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
@�B
@�B
@�B
@�B
A�B
@�B
@�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
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
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
M�B
N�B
O�B
O�B
N�B
N�B
N�B
O�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
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
\)B
\)B
\)B
\)B
]/B
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
`BB
`BB
`BB
`BB
`BB
`BB
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
ffB
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
k�B
k�B
k�B
k�B
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
p�B
p�B
p�B
p�B
p�B
q�B
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
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O� 	 ' )                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    , G�000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 PRES            TEMP            PSAL            NB_SAMPLE_CTD   PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.16 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230516172241                                          AO  ARCAADJP                                                                    20230516172241    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230516172241  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20230516172241  QCF$                G�O�G�O�G�O�0               