CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-18T16:40:21Z creation      
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  C�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  E�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  MH   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  O4   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  V�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ^�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  `�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  h0   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  j   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y|   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   NB_SAMPLE_CTD            
         	long_name         2Number of samples in each pressure bin for the CTD     
_FillValue        �     units         count      C_format      %5d    FORTRAN_format        I5     
resolution                �  ��   NB_SAMPLE_CTD_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  @  �x   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  8  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �@   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �P   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �T   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �d   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �l   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �pArgo profile    3.1 1.2 19500101000000  20230718164021  20230718164021  3902373 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL            NB_SAMPLE_CTD      A   AO  9469                            2B  A   APEX                            8948                            021122                          846 @���Y��1   @��ww�0@-]�-V�d�PH�1   GPS     Primary sampling: mixed [deep: discrete, shallow: averaged]                                                                                                                                                                                                        A   A   A       @�ff@�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B���B�  B���B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Cg�fCj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��C��C�  C��3C��3C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� DfD� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)fD)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>�fD?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dgy�Dg��Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtS3Dy��D���D�UqD�a�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��A�\A"�\AB�\Ad(�A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bi
=Bp��Bx��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B��B�Q�B��B�Q�B��B�Q�B��B��B��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�C (�C(�C(�C(�C(�C
(�C(�C(�C(�C(�CB�C(�C(�C(�C(�C(�C (�C"(�C$(�C&(�C((�C*(�C,(�C.(�C0(�C2(�C4(�C6(�C8(�C:(�C<(�C>(�C@(�CB(�CD(�CF(�CH(�CJ(�CL(�CN(�CP(�CR(�CT(�CV(�CX(�CZ(�C\(�C^(�C`(�Cb(�Cd(�Cf(�Ch\Cj(�Cl(�Cn(�Cp(�Cr(�Ct(�Cv(�Cx(�Cz(�C|B�C~B�C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C��C�{C�{C�{C�{C�!HC�!HC�{C�{C�{C�{C�{C�{C�!HC�!HC�{C��C��C�{C�{C�{C�{C�{C��C��C��C�{C�{C�{C�{C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{D 
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
=D��D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D�D�=D
=D�=D�D�=D
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
=D(�=D)�D)�=D*
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
=D>��D?
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
=Dg��Dh�Dh�=Di
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
=Dt]pDy��D��D�Z�D�g111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aѕ�Aћ�Aѥ�Aѝ�Aѡ�Aѡ�Aѡ�Aѡ�Aљ�AэPAсA�ZAΏ\A�r�A��Aͺ^A�|�A�ffA�Q�A�~�A�v�A�ffA�$�A̸RȀ\A�z�A�^5A���A�
=A�33A���A�hsA�  A�Q�A� �A�K�A�t�A��A��#A�7LA�C�A�p�A�l�A�$�A���A�JA�
=A�E�A��hA�p�A�+A�=qA�ȴA��`A� �A��PA�hsA��TA��A��9A�`BA�jA���A��!A�\)A��/A�Q�A�bA��hA��A�n�A�+A�x�A��A���A��A��yA���A��jA���A�Q�A��A���A���A�+A�A�A�ffA��9A}�PAy��Av��AtA�ApI�Al1Af~�Aa��A\��AV��AP��ALQ�AJ1AH�AF�yAD�HACp�AB�HAAO�A?�mA=��A;33A:�A:^5A7��A6�yA4��A2�jA0bA.�jA.  A,=qA+�A)��A( �A&9XA%�wA$~�A"�A"��A"�jA"�DA"^5A"VA!K�A jA�PAoA�9A=qA��A\)A��AM�A1'A`BA��A�A/A �A�DAM�A5?A�A;dAn�A�A��A�A?}AVAr�A1AZA�HA�DA
=A�A
 �A	|�An�A�^A�AffA��A�A z�@�v�@�1'@�+@�x�@���@�9X@�l�@��!@�-@�@��@�z�@�  @�@�
=@�ff@���@�bN@��u@�@�b@��@�5?@���@�@�O�@�7L@��@��`@�@�1@�!@�?}@�&�@��@��@�%@�%@�b@�-@�r�@�F@�E�@�ƨ@��@�=q@���@��`@��@߮@�@�@�1'@�ƨ@ۅ@�l�@�K�@��H@���@ڸR@ڸR@��@�9X@��`@��;@�
=@ڧ�@ڰ!@��y@ڧ�@�J@��T@�O�@ش9@؃@� �@�o@֟�@�~�@�V@�v�@֗�@ָR@��H@�~�@�J@թ�@���@ԃ@� �@Ӯ@�S�@�@�n�@Ѳ-@�hs@�G�@���@Ѓ@�1'@ϥ�@�;d@�@�ȴ@Η�@�?}@�A�@��@˅@ɩ�@�O�@��@Ȭ@�(�@��H@��@ź^@�p�@�X@���@þw@�+@�v�@�M�@�5?@��T@�O�@�1'@�l�@��@�$�@��#@��^@��@�(�@��w@��F@�;d@�ȴ@��+@��@��7@�`B@�/@�%@�I�@��P@��y@��@��@���@�z�@� �@��
@�+@��@��@��y@��H@���@�=q@���@��7@�x�@�hs@�`B@�X@�O�@�O�@�G�@��@���@���@��D@��F@�@���@��\@�v�@�v�@�M�@�E�@�5?@��^@��j@�A�@��
@�dZ@���@�-@�@��@��T@��@��@���@�O�@�z�@���@��P@�@��\@�@��@�&�@��u@�b@���@�C�@���@���@�~�@��T@��@�hs@�O�@�G�@��j@��u@��D@�I�@��
@�l�@���@��+@�v�@�ff@�=q@��-@�&�@���@��`@��@�9X@��@�b@���@�C�@��!@�5?@���@�Ĝ@�z�@�9X@��w@��@�|�@�l�@�\)@�S�@���@���@���@��!@��+@�n�@�M�@�5?@�{@��@���@��-@��7@�O�@�/@��@���@��D@�1@��@�C�@���@��R@�n�@�V@�M�@�E�@�=q@�5?@�-@�@��#@��7@��D@�A�@� �@�b@�  @��m@��;@���@���@�33@��H@��R@�{@��^@�p�@�O�@��@��j@� �@��F@��@�dZ@�\)@�"�@��\@�-@��@��h@�`B@�/@�i�@�	l@}�@s˒111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aѕ�Aћ�Aѥ�Aѝ�Aѡ�Aѡ�Aѡ�Aѡ�Aљ�AэPAсA�ZAΏ\A�r�A��Aͺ^A�|�A�ffA�Q�A�~�A�v�A�ffA�$�A̸RȀ\A�z�A�^5A���A�
=A�33A���A�hsA�  A�Q�A� �A�K�A�t�A��A��#A�7LA�C�A�p�A�l�A�$�A���A�JA�
=A�E�A��hA�p�A�+A�=qA�ȴA��`A� �A��PA�hsA��TA��A��9A�`BA�jA���A��!A�\)A��/A�Q�A�bA��hA��A�n�A�+A�x�A��A���A��A��yA���A��jA���A�Q�A��A���A���A�+A�A�A�ffA��9A}�PAy��Av��AtA�ApI�Al1Af~�Aa��A\��AV��AP��ALQ�AJ1AH�AF�yAD�HACp�AB�HAAO�A?�mA=��A;33A:�A:^5A7��A6�yA4��A2�jA0bA.�jA.  A,=qA+�A)��A( �A&9XA%�wA$~�A"�A"��A"�jA"�DA"^5A"VA!K�A jA�PAoA�9A=qA��A\)A��AM�A1'A`BA��A�A/A �A�DAM�A5?A�A;dAn�A�A��A�A?}AVAr�A1AZA�HA�DA
=A�A
 �A	|�An�A�^A�AffA��A�A z�@�v�@�1'@�+@�x�@���@�9X@�l�@��!@�-@�@��@�z�@�  @�@�
=@�ff@���@�bN@��u@�@�b@��@�5?@���@�@�O�@�7L@��@��`@�@�1@�!@�?}@�&�@��@��@�%@�%@�b@�-@�r�@�F@�E�@�ƨ@��@�=q@���@��`@��@߮@�@�@�1'@�ƨ@ۅ@�l�@�K�@��H@���@ڸR@ڸR@��@�9X@��`@��;@�
=@ڧ�@ڰ!@��y@ڧ�@�J@��T@�O�@ش9@؃@� �@�o@֟�@�~�@�V@�v�@֗�@ָR@��H@�~�@�J@թ�@���@ԃ@� �@Ӯ@�S�@�@�n�@Ѳ-@�hs@�G�@���@Ѓ@�1'@ϥ�@�;d@�@�ȴ@Η�@�?}@�A�@��@˅@ɩ�@�O�@��@Ȭ@�(�@��H@��@ź^@�p�@�X@���@þw@�+@�v�@�M�@�5?@��T@�O�@�1'@�l�@��@�$�@��#@��^@��@�(�@��w@��F@�;d@�ȴ@��+@��@��7@�`B@�/@�%@�I�@��P@��y@��@��@���@�z�@� �@��
@�+@��@��@��y@��H@���@�=q@���@��7@�x�@�hs@�`B@�X@�O�@�O�@�G�@��@���@���@��D@��F@�@���@��\@�v�@�v�@�M�@�E�@�5?@��^@��j@�A�@��
@�dZ@���@�-@�@��@��T@��@��@���@�O�@�z�@���@��P@�@��\@�@��@�&�@��u@�b@���@�C�@���@���@�~�@��T@��@�hs@�O�@�G�@��j@��u@��D@�I�@��
@�l�@���@��+@�v�@�ff@�=q@��-@�&�@���@��`@��@�9X@��@�b@���@�C�@��!@�5?@���@�Ĝ@�z�@�9X@��w@��@�|�@�l�@�\)@�S�@���@���@���@��!@��+@�n�@�M�@�5?@�{@��@���@��-@��7@�O�@�/@��@���@��D@�1@��@�C�@���@��R@�n�@�V@�M�@�E�@�=q@�5?@�-@�@��#@��7@��D@�A�@� �@�b@�  @��m@��;@���@���@�33@��H@��R@�{@��^@�p�@�O�@��@��j@� �@��F@��@�dZ@�\)@�"�@��\@�-@��@��h@�`B@�/@�i�@�	l@}�@s˒111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�yB	��B	�B	�`B	�ZB	�B
B
-B
.B
2-B
=qB
N�B
o�B
�=B
�LB
�BVBO�BW
Be`B^5BW
B}�B��B�B�;B�`BB+B+B�B�B��B�mB�3BffBbNBjBgmBs�B|�By�BgmB� B��B�?B�9B�qBȴBɺB��B�qB�dB�FB�B��B��B�VBz�BdZBO�BA�B0!B �B�B
��B
�fB
�B
��B
��B
��B
�DB
� B
q�B
^5B
G�B
DB	�B	��B	��B	��B	��B	�B	ffB	S�B	5?B	+B	#�B	,B	)�B	)�B	+B	&�B	%�B	$�B	$�B	�B	�B	�B	�B	�B	bB	PB	PB	�B	�B	�B	-B	"�B	�B	�B	�B	�B	%�B	"�B	.B	:^B	C�B	Q�B	dZB	n�B	k�B	bNB	aHB	]/B	W
B	R�B	N�B	H�B	@�B	9XB	8RB	6FB	2-B	,B	&�B	<jB	=qB	@�B	G�B	H�B	D�B	@�B	?}B	=qB	A�B	?}B	B�B	I�B	S�B	cTB	ffB	ZB	N�B	A�B	G�B	A�B	;dB	5?B	0!B	"�B	�B	JB		7B	+B	%B	B	B	B	B	%B	%B	+B	+B		7B	DB	JB	VB	uB	�B	33B	;dB	;dB	<jB	?}B	L�B	R�B	T�B	VB	T�B	T�B	T�B	S�B	T�B	XB	YB	ZB	ZB	ZB	[#B	[#B	_;B	_;B	\)B	\)B	\)B	`BB	^5B	^5B	^5B	_;B	_;B	_;B	`BB	bNB	cTB	cTB	dZB	cTB	e`B	iyB	l�B	m�B	p�B	u�B	�B	�VB	�hB	�hB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�3B	�3B	�9B	�LB	�qB	�wB	�wB	��B	��B	��B	��B	B	B	B	ÖB	ÖB	ĜB	ŢB	ŢB	ŢB	ŢB	ŢB	ƨB	ĜB	ÖB	ÖB	ŢB	ŢB	ǮB	ƨB	ƨB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�
B	�
B	�B	�B	�)B	�5B	�5B	�;B	�;B	�;B	�HB	�HB	�HB	�HB	�HB	�BB	�NB	�HB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
1B
+B
1B
	7B
	7B
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
JB
PB
PB
PB
PB
VB
VB
VB
\B
\B
\B
\B
bB
bB
hB
hB
bB
bB
oB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
!�B
!�B
!�B
"�B
*0B
/�B
72111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�yB	��B	�B	�`B	�ZB	�B
B
-B
.B
2-B
=qB
N�B
o�B
�=B
�LB
�BVBO�BW
Be`B^5BW
B}�B��B�B�;B�`BB+B+B�B�B��B�mB�3BffBbNBjBgmBs�B|�By�BgmB� B��B�?B�9B�qBȴBɺB��B�qB�dB�FB�B��B��B�VBz�BdZBO�BA�B0!B �B�B
��B
�fB
�B
��B
��B
��B
�DB
� B
q�B
^5B
G�B
DB	�B	��B	��B	��B	��B	�B	ffB	S�B	5?B	+B	#�B	,B	)�B	)�B	+B	&�B	%�B	$�B	$�B	�B	�B	�B	�B	�B	bB	PB	PB	�B	�B	�B	-B	"�B	�B	�B	�B	�B	%�B	"�B	.B	:^B	C�B	Q�B	dZB	n�B	k�B	bNB	aHB	]/B	W
B	R�B	N�B	H�B	@�B	9XB	8RB	6FB	2-B	,B	&�B	<jB	=qB	@�B	G�B	H�B	D�B	@�B	?}B	=qB	A�B	?}B	B�B	I�B	S�B	cTB	ffB	ZB	N�B	A�B	G�B	A�B	;dB	5?B	0!B	"�B	�B	JB		7B	+B	%B	B	B	B	B	%B	%B	+B	+B		7B	DB	JB	VB	uB	�B	33B	;dB	;dB	<jB	?}B	L�B	R�B	T�B	VB	T�B	T�B	T�B	S�B	T�B	XB	YB	ZB	ZB	ZB	[#B	[#B	_;B	_;B	\)B	\)B	\)B	`BB	^5B	^5B	^5B	_;B	_;B	_;B	`BB	bNB	cTB	cTB	dZB	cTB	e`B	iyB	l�B	m�B	p�B	u�B	�B	�VB	�hB	�hB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�3B	�3B	�9B	�LB	�qB	�wB	�wB	��B	��B	��B	��B	B	B	B	ÖB	ÖB	ĜB	ŢB	ŢB	ŢB	ŢB	ŢB	ƨB	ĜB	ÖB	ÖB	ŢB	ŢB	ǮB	ƨB	ƨB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�
B	�
B	�B	�B	�)B	�5B	�5B	�;B	�;B	�;B	�HB	�HB	�HB	�HB	�HB	�BB	�NB	�HB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
1B
+B
1B
	7B
	7B
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
JB
PB
PB
PB
PB
VB
VB
VB
\B
\B
\B
\B
bB
bB
hB
hB
bB
bB
oB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
!�B
!�B
!�B
"�B
*0B
/�B
72111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                              " , & "       " L d ? 0 "   7 5 * $                        % " $                                                                                                                                                                                                                                                                                                                                                                                                                             ����000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009999PRES            TEMP            PSAL            NB_SAMPLE_CTD   PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.16 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230718164021                                          AO  ARCAADJP                                                                    20230718164021    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230718164021  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20230718164021  QCF$                G�O�G�O�G�O�0               