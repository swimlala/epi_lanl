CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-03-18T18:38:14Z creation;2020-03-18T18:38:18Z conversion to V3.1;2022-11-21T05:27:23Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        h  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Il   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  MH   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  �X   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  �l   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20200318183814  20221123114511  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_204                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @�"�Hр1   @�#�β @:�'�/�W�dx�o h�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C�C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��H@�{A ��A ��A@��A`��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B (�B(�B(�B(�B (�B((�B0(�B8(�B@(�BH(�BP(�BX(�B`(�Bh(�Bp(�Bx(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�G�B�{B�{B�{B�{B�{B�{B�{B�{B�{C 
=C
=C#�C#�C#�C

=C
=C
=C
=C
=C
=C
=C
=C
=C
=C
=C 
=C"
=C$
=C&
=C(
=C*
=C,
=C.
=C0
=C2
=C4
=C6
=C8
=C:
=C<
=C>
=C@
=CB
=CD
=CF
=CH
=CJ
=CL
=CN
=CP
=CR
=CT
=CV
=CX
=CZ
=C\
=C^
=C`
=Cb
=Cd
=Cf
=Ch
=Cj
=Cl
=Cn
=Cp
=Cr
=Ct
=Cv
=Cx
=Cz
=C|
=C~
=C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D|)D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD�~D��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHDHD��HD�HD�AHDÁHD��HD�HD�AHDāHD��HD�HD�AHDŁHD��HD�HD�AHDƁHD��HD�HD�AHDǁHD��HD�HD�AHDȁHD��HD�HD�AHDɁHD��HD�HD�AHDʁHD��HD�HD�AHDˁHD��HD�HD�AHD́HD��HD�HD�AHD́HD��HD�HD�AHD΁HD��HD�HD�AHDρHD��HD�HD�AHDЁHD��HD�HD�AHDсHD��HD�HD�AHDҁHD��HD�HD�AHDӁHD��HD�HD�AHDԁHD��HD�HD�AHDՁHD��HD�HD�AHDցHD��HD�HD�AHDׁHD��HD�HD�AHD؁HD��HD�HD�AHDفHD��HD�HD�AHDځHD��HD�HD�AHDہHD��HD�HD�AHD܁HD��HD�HD�AHD݁HD��HD�HD�AHDށHD��HD�HD�AHD߁HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD��HD��{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ĜA�ƨA�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��7A�bNA�9XA���A��A�  A���A���A�O�A�+A��A�{A��A�r�A�Q�A��A�9XA�%A��yA�jA�1'A�1'A�t�A�&�A���A�  A��
A�+A��A�1A�x�A�bA�hsA�|�A���A��A��
A��jA��9A��A�~�A�~�A�l�A�K�A���A�VA�~�A�n�A�=qA�z�A�r�A�5?A���A�x�A�?}A�S�A�$�A�VA���A�"�A��mA�5?A���A��A�K�A��hA��A���A�bNA�r�A�S�A���A���A�I�A�A�=qA��RA�n�A�S�A���A�1'A���A�+A��A~��A}�A|n�AzA�Ay�Ax�HAx��AxbNAwƨAup�AtJAsXAp��Ao%Am�mAl��Ak�AjbAi&�Ag33Ae+AdffAc�;Ac��AcG�A`9XA]��A\�HA\ZAY��AXbAW33AT(�AR�AR  AP�/AP�AO�FAN�AN  AM�AKt�AJ�jAJ��AJ~�AJ(�AI�AH�jAF��AE%AC/A@��A@r�A@ZA?�A?\)A=�wA<bA;�mA;��A;�A:ĜA:ZA9x�A8�A8ĜA8��A8ZA8{A8  A7��A733A6��A5��A4z�A3�mA3�PA3oA2bA0��A0(�A/A/O�A//A.��A.�uA.r�A-�hA-+A,�jA,Q�A,1'A+�hA*n�A)A)|�A(�HA(v�A(1'A'�A&��A%��A%�7A%�A$ZA#VA"�\A!��A ��AVA��AXA�^Az�A�A�yA�+Al�A �A�HA  A��A��A"�A^5AJA�A��A��A+A1A\)A
ZAz�A;dA�
A+AK�At�A A�@�S�@�V@��@���@��@�p�@�&�@�Ĝ@�z�@�1@�F@�o@�p�@�(�@�C�@�5?@�hs@�w@�^@���@�\@���@�\)@�%@�t�@ޗ�@��#@ە�@���@�;d@�?}@�o@љ�@�V@�A�@�v�@�1@���@���@�%@��m@��@ř�@ÍP@��@�$�@��@�Ĝ@��u@�Z@�\)@���@���@�M�@�5?@���@�j@��m@�\)@�v�@�Q�@���@�5?@��@�X@�r�@��P@�@�V@���@�+@��@�{@��@�hs@�?}@�A�@�+@��R@��@���@��@���@�ƨ@�;d@�ff@�G�@��`@��j@�Z@��F@�"�@��@���@���@�@���@��`@��@�Q�@�ƨ@�@�=q@��@���@�p�@���@�1@���@�dZ@�@���@��!@��\@�=q@���@�Ĝ@�r�@�A�@� �@��@�ƨ@��w@��P@��@�n�@�5?@���@��T@���@���@��h@���@�9X@���@��w@�l�@���@�v�@��#@�&�@��9@��D@��D@��@�Q�@��@��@���@�l�@�@���@���@��\@�v�@�V@�=q@�5?@�5?@�5?@�J@��#@��^@���@��@���@�Z@��@��R@�E�@��h@�Ĝ@�Z@�9X@��w@�C�@�"�@���@�^5@�J@���@�hs@��@���@��9@���@��@�r�@�1'@�b@�  @�  @�A�@� �@�  @�ƨ@��@�C�@��@�^5@�&�@���@��9@���@���@��u@��u@��@�Z@�b@�(�@�A�@�A�@�1'@� �@�1@��@�P@\)@
=@~�R@~V@}?}@|Z@{o@z�\@z^5@z^5@z-@y�^@y&�@x��@xb@w�@w��@w|�@v�+@u`B@tj@s��@s"�@r^5@q��@q��@q�@pr�@p �@o��@o�P@o;d@o�@n�@n��@nE�@n$�@n@m�T@m��@m�h@m�h@m�h@m�@l�j@l(�@kdZ@j��@j~�@jJ@i��@i�@h�`@hbN@h1'@g�@g|�@f�@f�R@f��@fv�@f5?@e�-@e��@e?}@d�/@d�j@dI�@d�@c�m@cƨ@c��@c�@c33@b�H@b��@b-@ahs@`�9@`�u@`r�@`Q�@_�;@_|�@^��@^��@^ff@]�T@]�-@]�h@]`B@\��@\�j@\�j@\��@\j@\9X@\(�@\1@[��@[�
@[�
@[�F@[��@[��@[dZ@Z�\@ZJ@Y�#@Y�^@Y��@Y��@Yx�@YG�@Y7L@Y%@XĜ@X�9@X�@XA�@Xb@W�;@W|�@Vff@V{@U��@U�-@U�@T�/@T��@S�m@S�@SdZ@R�H@R-@Q��@Q�7@QX@P��@P��@PbN@O�w@O��@O�P@Ol�@O�@N�y@Nv�@N{@M@Mp�@L��@K�m@K��@K��@KC�@J�!@J^5@J=q@J-@I�^@I&�@H��@HbN@H �@G�;@G�@G;d@F�y@F�@F��@E�T@E�T@E��@E��@E@E�-@E�h@E�h@E�@E`B@D��@D��@Dj@D1@C��@C@Bn�@A��@A��@A�7@A�7@@��@@Ĝ@@��@@r�@?�;@?��@?�@>��@>�+@>v�@>V@>$�@=��@=�@=?}@<�@<�j@<�@<�D@<j@<I�@;��@;"�@:��@:�!@:n�@:-@:�@9�^@9x�@9&�@8r�@7�@8b@7�;@7l�@7�@7
=@6��@65?@5��@5O�@4�@4�@4��@4z�@4Z@4Z@4Z@4(�@3��@3ƨ@3�F@3��@3@2�H@2�!@2~�@1��@1�^@1�7@1G�@0�`@0Q�@/�@/��@/�w@/�@/�P@/|�@/\)@/+@.��@.�R@.$�@-`B@-/@,�@,�@,�@,�@,�/@,�/@,�/@,��@,z�@,(�@+�
@+��@+S�@+o@+@*��@*~�@*M�@*-@)��@)�@)��@)�7@)7L@(��@(r�@( �@'��@'|�@'\)@'+@&�y@&��@&V@&5?@&$�@&{@%�@%�T@%p�@$�@$��@$9X@$(�@$1@#�F@#dZ@#o@"��@"�!@"��@"n�@"M�@"�@!�^@!hs@!X@!X@!X@!X@!G�@!�@!%@ Ĝ@ A�@   @��@�@��@l�@�y@��@��@��@ff@5?@@@{@@@�-@`B@?}@�@��@j@ƨ@ƨ@ƨ@ƨ@�F@�@S�@S�@C�@"�@@��@^5@=q@J@��@x�@7L@�@��@�9@�@bN@1'@  @�;@�@�P@\)@��@ȴ@ȴ@ȴ@�R@�+@ff@E�@5?@�@��@��@�h@�@�@`B@?}@/@��@�@��@��@z�@Z@I�@I�@Z@9X@(�@�@�@1@�m@ƨ@C�@��@^5@=q@�@�@��@��@x�@x�@hs@&�@Ĝ@��@�@b@  @��@�w@�@�P@�P@|�@l�@l�@\)@\)@;d@�y@ȴ@v�@5?@{@@�@@�@O�@?}@�@�@�D@Z@�@��@ƨ@��@�@"�@@
��@
M�@
-@
J@	��@	hs@	G�@	7L@	&�@	%@Ĝ@�u@�@Q�@1'@ �@  @  @  @��@��@K�@;d@+@�@�y@ȴ@�+@5?@$�@{@@�T@��@�-@�h@p�@O�@�@�@��@�j@�@��@z�@I�@�@�m@�
@��@t�@dZ@S�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ĜA�ƨA�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��7A�bNA�9XA���A��A�  A���A���A�O�A�+A��A�{A��A�r�A�Q�A��A�9XA�%A��yA�jA�1'A�1'A�t�A�&�A���A�  A��
A�+A��A�1A�x�A�bA�hsA�|�A���A��A��
A��jA��9A��A�~�A�~�A�l�A�K�A���A�VA�~�A�n�A�=qA�z�A�r�A�5?A���A�x�A�?}A�S�A�$�A�VA���A�"�A��mA�5?A���A��A�K�A��hA��A���A�bNA�r�A�S�A���A���A�I�A�A�=qA��RA�n�A�S�A���A�1'A���A�+A��A~��A}�A|n�AzA�Ay�Ax�HAx��AxbNAwƨAup�AtJAsXAp��Ao%Am�mAl��Ak�AjbAi&�Ag33Ae+AdffAc�;Ac��AcG�A`9XA]��A\�HA\ZAY��AXbAW33AT(�AR�AR  AP�/AP�AO�FAN�AN  AM�AKt�AJ�jAJ��AJ~�AJ(�AI�AH�jAF��AE%AC/A@��A@r�A@ZA?�A?\)A=�wA<bA;�mA;��A;�A:ĜA:ZA9x�A8�A8ĜA8��A8ZA8{A8  A7��A733A6��A5��A4z�A3�mA3�PA3oA2bA0��A0(�A/A/O�A//A.��A.�uA.r�A-�hA-+A,�jA,Q�A,1'A+�hA*n�A)A)|�A(�HA(v�A(1'A'�A&��A%��A%�7A%�A$ZA#VA"�\A!��A ��AVA��AXA�^Az�A�A�yA�+Al�A �A�HA  A��A��A"�A^5AJA�A��A��A+A1A\)A
ZAz�A;dA�
A+AK�At�A A�@�S�@�V@��@���@��@�p�@�&�@�Ĝ@�z�@�1@�F@�o@�p�@�(�@�C�@�5?@�hs@�w@�^@���@�\@���@�\)@�%@�t�@ޗ�@��#@ە�@���@�;d@�?}@�o@љ�@�V@�A�@�v�@�1@���@���@�%@��m@��@ř�@ÍP@��@�$�@��@�Ĝ@��u@�Z@�\)@���@���@�M�@�5?@���@�j@��m@�\)@�v�@�Q�@���@�5?@��@�X@�r�@��P@�@�V@���@�+@��@�{@��@�hs@�?}@�A�@�+@��R@��@���@��@���@�ƨ@�;d@�ff@�G�@��`@��j@�Z@��F@�"�@��@���@���@�@���@��`@��@�Q�@�ƨ@�@�=q@��@���@�p�@���@�1@���@�dZ@�@���@��!@��\@�=q@���@�Ĝ@�r�@�A�@� �@��@�ƨ@��w@��P@��@�n�@�5?@���@��T@���@���@��h@���@�9X@���@��w@�l�@���@�v�@��#@�&�@��9@��D@��D@��@�Q�@��@��@���@�l�@�@���@���@��\@�v�@�V@�=q@�5?@�5?@�5?@�J@��#@��^@���@��@���@�Z@��@��R@�E�@��h@�Ĝ@�Z@�9X@��w@�C�@�"�@���@�^5@�J@���@�hs@��@���@��9@���@��@�r�@�1'@�b@�  @�  @�A�@� �@�  @�ƨ@��@�C�@��@�^5@�&�@���@��9@���@���@��u@��u@��@�Z@�b@�(�@�A�@�A�@�1'@� �@�1@��@�P@\)@
=@~�R@~V@}?}@|Z@{o@z�\@z^5@z^5@z-@y�^@y&�@x��@xb@w�@w��@w|�@v�+@u`B@tj@s��@s"�@r^5@q��@q��@q�@pr�@p �@o��@o�P@o;d@o�@n�@n��@nE�@n$�@n@m�T@m��@m�h@m�h@m�h@m�@l�j@l(�@kdZ@j��@j~�@jJ@i��@i�@h�`@hbN@h1'@g�@g|�@f�@f�R@f��@fv�@f5?@e�-@e��@e?}@d�/@d�j@dI�@d�@c�m@cƨ@c��@c�@c33@b�H@b��@b-@ahs@`�9@`�u@`r�@`Q�@_�;@_|�@^��@^��@^ff@]�T@]�-@]�h@]`B@\��@\�j@\�j@\��@\j@\9X@\(�@\1@[��@[�
@[�
@[�F@[��@[��@[dZ@Z�\@ZJ@Y�#@Y�^@Y��@Y��@Yx�@YG�@Y7L@Y%@XĜ@X�9@X�@XA�@Xb@W�;@W|�@Vff@V{@U��@U�-@U�@T�/@T��@S�m@S�@SdZ@R�H@R-@Q��@Q�7@QX@P��@P��@PbN@O�w@O��@O�P@Ol�@O�@N�y@Nv�@N{@M@Mp�@L��@K�m@K��@K��@KC�@J�!@J^5@J=q@J-@I�^@I&�@H��@HbN@H �@G�;@G�@G;d@F�y@F�@F��@E�T@E�T@E��@E��@E@E�-@E�h@E�h@E�@E`B@D��@D��@Dj@D1@C��@C@Bn�@A��@A��@A�7@A�7@@��@@Ĝ@@��@@r�@?�;@?��@?�@>��@>�+@>v�@>V@>$�@=��@=�@=?}@<�@<�j@<�@<�D@<j@<I�@;��@;"�@:��@:�!@:n�@:-@:�@9�^@9x�@9&�@8r�@7�@8b@7�;@7l�@7�@7
=@6��@65?@5��@5O�@4�@4�@4��@4z�@4Z@4Z@4Z@4(�@3��@3ƨ@3�F@3��@3@2�H@2�!@2~�@1��@1�^@1�7@1G�@0�`@0Q�@/�@/��@/�w@/�@/�P@/|�@/\)@/+@.��@.�R@.$�@-`B@-/@,�@,�@,�@,�@,�/@,�/@,�/@,��@,z�@,(�@+�
@+��@+S�@+o@+@*��@*~�@*M�@*-@)��@)�@)��@)�7@)7L@(��@(r�@( �@'��@'|�@'\)@'+@&�y@&��@&V@&5?@&$�@&{@%�@%�T@%p�@$�@$��@$9X@$(�@$1@#�F@#dZ@#o@"��@"�!@"��@"n�@"M�@"�@!�^@!hs@!X@!X@!X@!X@!G�@!�@!%@ Ĝ@ A�@   @��@�@��@l�@�y@��@��@��@ff@5?@@@{@@@�-@`B@?}@�@��@j@ƨ@ƨ@ƨ@ƨ@�F@�@S�@S�@C�@"�@@��@^5@=q@J@��@x�@7L@�@��@�9@�@bN@1'@  @�;@�@�P@\)@��@ȴ@ȴ@ȴ@�R@�+@ff@E�@5?@�@��@��@�h@�@�@`B@?}@/@��@�@��@��@z�@Z@I�@I�@Z@9X@(�@�@�@1@�m@ƨ@C�@��@^5@=q@�@�@��@��@x�@x�@hs@&�@Ĝ@��@�@b@  @��@�w@�@�P@�P@|�@l�@l�@\)@\)@;d@�y@ȴ@v�@5?@{@@�@@�@O�@?}@�@�@�D@Z@�@��@ƨ@��@�@"�@@
��@
M�@
-@
J@	��@	hs@	G�@	7L@	&�@	%@Ĝ@�u@�@Q�@1'@ �@  @  @  @��@��@K�@;d@+@�@�y@ȴ@�+@5?@$�@{@@�T@��@�-@�h@p�@O�@�@�@��@�j@�@��@z�@I�@�@�m@�
@��@t�@dZ@S�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B}�B|�B|�B|�B|�B}�B}�B}�B~�B~�B�B�B�1B�PB�\B�hB�oB��B��B��B��B��B��B��B��B��B�uB�=B�%B�By�Bo�Bk�BgmB`BBXBR�BM�BI�BD�B@�B9XB0!B!�B�B�B�B�B�B�B�B{BoB\B	7BB��B�ZB�/B�B��B�wB�!B��B�{B�+Bw�Bp�BjBe`BXB?}B6FB,B!�B�B
=B
��B
�sB
�fB
�5B
��B
ǮB
�}B
�RB
�'B
�B
��B
��B
��B
�hB
�=B
�B
~�B
w�B
m�B
_;B
XB
VB
S�B
P�B
K�B
<jB
33B
,B
�B
bB
	7B
B	��B	�B	�sB	�5B	��B	��B	��B	��B	ɺB	�jB	�B	�B	��B	��B	�{B	�PB	~�B	w�B	s�B	n�B	jB	gmB	cTB	^5B	ZB	T�B	Q�B	Q�B	P�B	O�B	L�B	H�B	>wB	6FB	/B	#�B	"�B	!�B	�B	�B	�B	PB	DB	
=B	+B	B	B��B��B��B��B��B��B��B��B��B�B�B�yB�fB�`B�TB�5B�B�B��B��B��B��B��B��BǮBƨBƨBĜBÖB��B�jB�XB�LB�?B�3B�-B�B��B��B��B��B��B��B��B��B�hB�1B�Bz�Bw�Bt�Bq�Bo�Bm�BiyBe`BbNB_;B]/BZBXBW
BVBT�BT�BS�BQ�BO�BL�BI�BF�BB�B@�B=qB9XB6FB33B1'B/B.B.B-B-B-B,B,B,B+B)�B(�B'�B&�B&�B%�B$�B#�B"�B!�B!�B �B �B �B�B�B�B�B�B�B�B�B�B�B�B!�B"�B"�B"�B$�B$�B%�B(�B(�B)�B,B,B,B,B-B.B.B.B.B.B0!B0!B1'B2-B6FB6FB8RB9XB9XB:^B<jB?}BA�BF�BG�BH�BJ�BL�BL�BL�BM�BN�BO�BO�BO�BO�BP�BR�BS�BW
B]/B_;B`BBbNBe`BgmBl�Bm�Bm�Bm�Bo�Bs�Bv�Bw�Bx�By�By�B|�B}�B~�B�B�B�B�B�B�%B�1B�7B�7B�DB�\B�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�9B�?B�XB�qB��B��B��BBÖBĜBŢBǮBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�/B�NB�`B�yB�B�B�B��B��B��B��B	B	1B	
=B	DB	\B	bB	hB	hB	oB	uB	�B	�B	�B	 �B	$�B	$�B	%�B	(�B	,B	0!B	49B	5?B	6FB	6FB	7LB	8RB	8RB	8RB	8RB	9XB	;dB	A�B	G�B	I�B	J�B	K�B	L�B	M�B	N�B	O�B	P�B	Q�B	R�B	S�B	XB	]/B	aHB	bNB	cTB	cTB	dZB	e`B	gmB	iyB	jB	jB	l�B	m�B	q�B	u�B	y�B	|�B	}�B	�B	�B	�B	�B	�1B	�7B	�=B	�DB	�JB	�PB	�PB	�VB	�\B	�bB	�hB	�oB	�uB	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�'B	�-B	�9B	�?B	�?B	�?B	�FB	�FB	�RB	�XB	�^B	�jB	�qB	��B	��B	��B	��B	B	ĜB	ŢB	ƨB	ƨB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�5B	�;B	�BB	�BB	�NB	�NB	�TB	�`B	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
%B
%B
1B
	7B
	7B

=B
JB
PB
PB
PB
VB
VB
\B
\B
bB
bB
hB
hB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
$�B
%�B
&�B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
-B
-B
-B
-B
.B
/B
/B
0!B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
49B
49B
49B
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
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
@�B
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
B�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
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
I�B
J�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
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
XB
XB
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
ZB
ZB
ZB
[#B
[#B
[#B
[#B
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
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
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
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
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
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
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
o�B
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
t�B
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
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B}�B|�B|�B|�B|�B~B~B~BBHBcB��B�aB�B��B��B��B�&B��B��B��B�B��B�hB�ZB�~B��B��B�^B�+B�B{JBp�Bl�BiyBb�BY�BTFBN�BJ�BE�BB[B;�B3MB#nB�B�B�B�B�B�B�B2B�B�BBB��B�B�5B�/B�B��B��B�&B�$B�RBy	Bq�Bk�BgmB[	BAoB7�B-�B#�BB�B
��B
�DB
��B
��B
ӏB
�B
��B
�rB
��B
��B
�B
�zB
��B
��B
�)B
�?B
�OB
y�B
o�B
`vB
XyB
VmB
T�B
R:B
N<B
>(B
4�B
.�B
�B
�B

�B
�B	��B	�B	��B	�'B	��B	ЗB	ΊB	�<B	�6B	�B	�UB	�wB	��B	��B	�SB	�}B	��B	x�B	uB	o�B	k6B	h�B	d�B	_�B	[�B	U�B	R:B	R:B	Q�B	P�B	NVB	KDB	@�B	8�B	1vB	$@B	#:B	"�B	 �B	�B	$B	�B	�B	
�B	�B	�B	B�}B�BB�BB�<B�6B�*B�XB��B��B��B��B�0B�B�LB��B߾B��B֡BԕB�:B�hB�HB�VB��B�1B�EB�EB�BĜB��B�<B��B�B��B��B�B�UB��B��B��B�B�TB��B��B��B�FB�XB��B|�ByXBu�Br�Bp�Bo5Bk6BgBc�B`�B^�B[#BX�BWsBVSBUMBUgBT�BSuBQBN�BLBH�BDMBBB?�B;�B8lB6+B2�B0B.�B.}B-�B-]B-]B,WB,qB,qB+�B+B)�B(�B'�B'�B'8B&LB%B#�B"�B#B"NB!�B!�B �B vB �B B BB 'B �B \B �B 'B!HB"�B#�B#�B#�B%�B%�B'B)_B)�B*�B,WB,=B,qB,�B-wB.IB.cB.IB.�B.�B0�B0�B2B3�B6�B72B8�B9�B:*B;B=�B@4BBuBGBHBI7BK)BMBMBM�BN�BOBBPbBP.BPHBP�BQ�BS[BT�BW�B]~B_pB`�Bb�Be�Bh
Bl�Bm�Bm�Bm�Bp!Bs�BwBxRByrBz^BzB}"B~]BcB�oB�aB�mB�mB�9B�YB�fB��B��B��B��B��B��B��B��B��B��B�B�B��B�B��B��B�B�,B��B�KB�CB�IB��B��B�nB��B��B��B��B��B��BªB��B��B��B��B�B�B��B��B��B��B��B��B� B� B� B�&B�B�2B�gB�sB�eB��B��B�B��B�B��B��B�B�2B��B�B�HB	MB	fB	
�B	xB	�B	}B	hB	�B	oB	�B	�B	�B	�B	 �B	$�B	%B	&B	)B	,WB	0�B	4�B	5�B	6`B	6`B	7fB	8lB	8lB	8lB	8lB	9�B	;�B	AoB	G�B	I�B	J�B	K�B	L�B	M�B	N�B	O�B	Q B	RB	S&B	TFB	XyB	]�B	a|B	bhB	cTB	cnB	dtB	e�B	g�B	i�B	j�B	j�B	l�B	m�B	rB	v+B	z*B	}"B	~BB	�;B	�'B	�GB	�SB	�1B	�7B	�=B	�^B	�dB	�jB	�jB	�pB	�vB	�}B	�hB	��B	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�
B	��B	�B	�B	�0B	�B	�)B	�OB	�'B	�AB	�GB	�TB	�ZB	�ZB	�ZB	�FB	�zB	�RB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	żB	��B	��B	��B	ɺB	ɺB	��B	��B	ˬB	��B	��B	��B	��B	��B	��B	��B	οB	��B	��B	��B	��B	�4B	�&B	��B	�B	�B	��B	�B	�B	�B	�B	�$B	�$B	�+B	�B	�1B	�1B	�QB	یB	�OB	�VB	�BB	�\B	�NB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�	B	��B	��B	�B	�"B	�B	�B
 B
 B
;B
'B
B
9B
B
?B
YB
KB
	RB
	RB

rB
0B
PB
PB
PB
pB
pB
\B
\B
bB
}B
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
$�B
&B
'8B
(
B
)B
*B
*B
)�B
*0B
*B
*0B
+6B
+6B
,B
,B
-CB
-)B
-B
-CB
.IB
/5B
/5B
0;B
1AB
2-B
2GB
2GB
2-B
2-B
2GB
3MB
3MB
49B
4TB
4nB
5ZB
5?B
5ZB
5tB
6`B
6FB
7fB
7�B
7�B
8lB
9rB
9XB
9XB
:xB
:xB
:^B
:^B
:^B
;B
;�B
;�B
=qB
=�B
>wB
>]B
>wB
>wB
>wB
>wB
>wB
>�B
?�B
@�B
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
B�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
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
I�B
J�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
OB
O�B
O�B
O�B
O�B
O�B
O�B
O�B
Q B
Q B
QB
RB
Q�B
RB
Q�B
RB
S&B
R�B
S�B
S�B
S�B
TB
TB
T�B
T�B
T�B
UB
UB
UB
VB
VB
VB
VB
W?B
XB
XB
XB
XB
X+B
X+B
XB
XB
X+B
XB
YB
Y1B
Z7B
ZB
Z7B
[=B
[=B
[=B
[=B
\CB
\CB
\CB
\CB
]IB
]IB
]IB
]IB
^OB
^OB
^OB
_;B
_;B
_VB
_VB
_VB
_;B
_VB
_VB
`\B
`\B
`BB
a-B
aHB
abB
abB
abB
abB
abB
bhB
bNB
bNB
bhB
bNB
b4B
bNB
bNB
c:B
cTB
cTB
cTB
cnB
cTB
c�B
d�B
dtB
e`B
ezB
ezB
f�B
f�B
f�B
fLB
ffB
f�B
gmB
g�B
g�B
h�B
hsB
hsB
hXB
hsB
i_B
iyB
iyB
iyB
iyB
iyB
iyB
i�B
iyB
j�B
j�B
jB
j�B
k�B
k�B
k�B
k�B
k�B
k�B
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
o�B
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
t�B
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
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.04(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202003290034172020032900341720200329003417202211182142272022111821422720221118214227202003300018232020033000182320200330001823  JA  ARFMdecpA19c                                                                20200319033740  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200318183814  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200318183817  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200318183817  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200318183818  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200318183818  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200318183818  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20200318183818  QCF$                G�O�G�O�G�O�            8000JA  ARGQaqcp2.8e                                                                20200318183818  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200318183818  QCF$                G�O�G�O�G�O�            8000JA  ARGQrqcpt16c                                                                20200318183818  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200318183818                      G�O�G�O�G�O�                JA  ARUP                                                                        20200318185333                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20200319153510  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQJMQC2.0                                                                 20200319153434  CV  JULD            G�O�G�O�F�Y                JM  ARCAJMQC2.0                                                                 20200328153417  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200328153417  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200329151823  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118124227  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123114511                      G�O�G�O�G�O�                