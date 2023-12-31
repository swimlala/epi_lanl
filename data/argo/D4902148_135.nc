CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-04-28T15:36:29Z creation;2018-04-28T15:36:32Z conversion to V3.1;2019-12-18T07:23:18Z update;2022-11-21T05:30:52Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
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
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]T   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180428153629  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_135                     2C  DdKxNAVIS_A                         0397                            ARGO 011514                     863 @�^�+��1   @�^��}( @<�:)�y��dKxF�]1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @��@�33@ə�@���A   A@  A`  A~ffA�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�(�@�R@�@�(�@�\)A!G�AAG�AaG�A�A���A�p�A���A���AУ�A��A��B Q�BQ�BQ�BQ�B Q�B(Q�B0Q�B8Q�B@Q�BHQ�BPQ�BXQ�B`Q�Bh�RBpQ�BxQ�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B���B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C {C{C{C{C{C
{C{C{C{C{C{C{C{C{C{C{C {C"{C${C&{C({C*{C,{C.{C0{C2{C4{C6{C8{C:{C<{C>{C@{CB{CD{CF{CH{CJ{CL{CN{CP{CR{CT{CV{CX{CZ{C\{C^{C`{Cb{Cd{Cf{Ch{Cj{Cl{Cn{Cp{Cr{Ct{Cv{Cx{Cz{C|{C~{C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=D D �DD�DD�DD�DD�DD�D�D�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD~�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�DuDu�DvDv�DwDw�DxDx�DyDy�DzDz�D{D{�D|D|�D}D}�D~D~�DD�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D�D�D��D�B�DÂ�D�D��D�B�DĂ�D�D��D�B�Dł�D�D��D�B�DƂ�D�D��D�B�Dǂ�D�D��D�B�DȂ�D�D��D�B�Dɂ�D�D��D�B�Dʂ�D�D��D�B�D˂�D�D��D�B�D̂�D�D��D�B�D͂�D�D��D�B�D΂�D�D��D�B�Dς�D�D��D�B�DЂ�D�D��D�B�Dт�D�D��D�B�D҂�D�D��D�B�Dӂ�D�D��D�B�DԂ�D�D��D�B�DՂ�D�D��D�B�Dւ�D�D��D�B�Dׂ�D�D��D�B�D؂�D�D��D�B�Dق�D�D��D�B�Dڂ�D�D��D�B�Dۂ�D�D��D�B�D܂�D�D��D�B�D݂�D�D��D�B�Dނ�D�D��D�B�D߂�D�D��D�B�D���D�D��D�B�DႏD�D��D�B�D₏D�D��D�B�DわD�D��D�B�D䂏D�D��D�B�D傏D�D��D�B�D悏D�D��D�B�D炏D�D��D�B�D肏D�D��D�B�D邏D�D��D�B�DꂏD�D��D�B�D낏D�D��D�B�D삏D�D��D�B�D킏D�D��D�B�DD�D��D�B�DD�D��D�B�D���D�D��D�B�D�D�D��D�B�D�D�D��D�B�D�D�D��D�B�D�D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�E�D���D�D��D�B�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�ĜA���A��A��A���A�A�%A�1A�
=A�JA�A�
=A�1A��A��TA�(�A�oA���A��!A�O�A�A�5?A��A��`A��
A��A��7A��A�K�A��A��A�I�A�VA�ĜA�I�A���A�`BA�A�A��A���A���A���A���A���A�`BA��yA��uA���A�1A�z�A��\A�+A���A���A���A��TA��/A�r�A�A�x�A�^5A��A�%A�?}A�?}A��/A��DA�JA�x�A���A��#A��A���A��A�%A�z�A���A�O�A~�jA|�+Az(�AxffAw"�Av��Av$�Au;dAs�ArffAq�Aop�Anz�Am�7Am+Al��Al  AkK�Aj�AiƨAh��Ahz�Ag�mAg?}AfĜAfz�Af �AeAe\)Ad�Adr�Ad5?Ad�AdAc�Ac�#Ac��Ac+AbȴAb�jAb�jAb��AbJAa�Aa\)A`��A`jA_�A^��A]��AZVAV�AT��AT�uAS��ARZAQXAPM�AO�AN��AM�AM+AL�AKS�AJ�/AJr�AJ5?AI�;AIl�AH�AG��AFn�ADE�AC�AB�AB�AB^5AB1'AA��AAG�A@��A@�!A@ffA?�
A?33A?
=A>�A>�9A> �A<A�A9��A933A8��A81'A81A7�-A7x�A7A4n�A2�9A1t�A0�HA0jA0 �A/33A.bA-l�A,�A*I�A(��A&��A%A$JA"VA!��A!A!�FA!�PA ��A�7AK�A��A�A&�A�!AA��A��AffAAl�AbNA�9A1A�\A��A�jAVA-A  AȴAA��A��A  A�A7LA
ZA	��A	�PA	��A	�hA	�A�A�\AbNA1'A�;A��AZA�#A33A&�AE�A�
A��A�A j@�o@�5?@�Z@���@�r�@�\@�Z@�C�@�~�@�hs@�j@��@��
@�@��H@�V@�@�/@�Q�@��@�dZ@��@��@䛦@�@�dZ@�33@��@��@��@���@�%@�1'@��@��H@��T@�?}@���@�V@�O�@�  @���@�&�@��;@ʰ!@ǝ�@�^5@ř�@� �@�|�@��@��w@�;d@�
=@�^5@��-@�`B@�1'@���@��H@��@���@�dZ@��@��y@���@�V@��@�{@��T@��^@��7@���@�A�@��@�
=@���@���@�K�@�5?@�hs@��R@��@�o@��!@�=q@��@��@�$�@��@�{@���@��@���@���@�?}@�j@�C�@�
=@��@���@��w@���@�n�@�Ĝ@��+@��T@�@���@�x�@�7L@��`@�bN@�t�@���@���@���@��@��@��#@��-@��@�p�@�O�@�?}@�&�@��@�V@��@�Z@���@�b@�C�@���@�-@��^@�G�@��j@�A�@�1@���@���@�l�@�\)@�\)@�+@�
=@���@�ff@�$�@��@��T@�@��h@�x�@�hs@�X@�O�@�?}@��@���@���@���@��m@�J@��F@��@�t�@�\)@�;d@�
=@�^5@�%@���@�A�@��@�b@�b@�b@�1@�b@�1@�1@��@��m@��;@���@��P@�|�@�l�@�dZ@�\)@�S�@�33@�
=@��!@�{@��-@�V@���@���@��/@���@���@�bN@�  @\)@~v�@}�@}V@|z�@|1@{dZ@y�#@wl�@u�T@u�-@up�@u�@t�/@t��@t�j@t��@t�D@tz�@tj@tZ@t9X@t9X@t9X@t(�@t(�@t�@s�m@sƨ@s�F@s��@sdZ@so@r�!@r-@r�@q�#@q�^@q��@q��@q&�@p��@p �@o�@o|�@o\)@o�@n��@n�y@nȴ@n�+@m��@m/@lj@l1@k��@kt�@k"�@j�@j��@j�!@j^5@i��@h��@hr�@hr�@hA�@hA�@h �@g�w@gl�@f�y@f5?@ep�@d�@dz�@dZ@d(�@c��@cS�@co@c@b�@b�H@b��@b��@b��@b��@b��@b�!@b�!@b��@b��@b��@b�!@b�@a�^@a��@ax�@a&�@`��@`r�@`1'@`  @_�@_;d@^��@^v�@^{@]`B@\�@\�D@\I�@\�@[ƨ@[t�@Z��@Y��@Y��@X�@XbN@Xr�@XQ�@V�y@V@U��@U�h@U�h@Up�@U`B@UO�@U�@U�@T�/@Tz�@Tz�@T(�@S��@S�F@S�F@S�F@S��@S"�@R��@R~�@R�@Q�^@Qhs@Q&�@P�`@PĜ@Pr�@PQ�@PA�@Pb@P  @O��@N��@NE�@N@M@M�@M/@MV@L�/@L�D@Lz�@L9X@K�m@K��@K��@K�@KS�@J�!@J^5@JJ@I�@I�^@IG�@H�`@HbN@H �@H  @G��@G�@G\)@GK�@G;d@G�@G
=@FE�@E��@D�j@D�@C�F@C��@C��@C�@C33@B�H@B��@B~�@BJ@Ahs@A�@@��@@r�@@A�@@b@?�@?�;@?�w@?�@?K�@>�y@>��@>�+@>5?@=�h@=�@<�@<z�@<j@<I�@;��@;ƨ@;t�@;t�@;dZ@;S�@;"�@:�H@9��@9X@9G�@9�@8Ĝ@8�u@8�@8r�@8Q�@8A�@8 �@8  @7�@7�;@7��@7�@7�P@7|�@6��@6��@6��@6��@6ff@6$�@6@5��@5�h@5p�@4��@4��@4z�@3�
@3C�@2�\@1�7@1�@0��@0�9@0r�@01'@/|�@/�@/
=@.��@.�@.��@.ff@.@-@-�h@-p�@-V@,Z@,(�@+�F@+��@+�@+dZ@+C�@+33@+o@*��@*�\@*^5@*M�@*=q@*�@*J@)�@)�#@)��@)�7@)X@(�`@(�u@'�;@'|�@';d@'
=@&�y@&�@&�R@&��@&ff@&V@&E�@&$�@%�@%�T@%�h@%O�@$��@#��@#dZ@#C�@#33@"�@"��@"^5@!��@!�@!�#@!��@!x�@!hs@!X@!G�@!�@ ��@ �9@ ��@ ��@ ��@ �u@ bN@ 1'@ b@��@�@�y@ȴ@ff@V@V@5?@{@@��@O�@��@�j@z�@I�@(�@�@�@�@1@1@ƨ@t�@C�@o@@�H@�!@n�@=q@J@J@��@��@��@��@��@x�@&�@�@�`@Ĝ@��@�@bN@A�@ �@�@��@�;@��@��@\)@�@�y@�@��@�+@ff@5?@{@�@@�@�@p�@?}@/@��@��@I�@9X@9X@9X@9X@��@�
@�F@�@C�@�@��@��@~�@M�@=q@-@J@�#@��@7L@��@��@�`@Ĝ@��@�@bN@Q�@1'@�;@�P@\)@�@��@�y@�@�R@��@��@�+@�+@�+@ff@V@E�@5?@{@�@�@�T@��@��@O�@/@V@��@�/@��@�D@z�@z�@j@I�@(�@�m@��@�@S�@C�@@
�\@
M�@	�#@	��@	x�@	G�@	7L@	7L@	&�@	�@	�@	�@	%@��@�`@�9@ �@��@�P@l�@;d@
=@ȴ@�R@�R@��@��@�+@v�@ff@V@V@E�@5?@@�h@O�@O�@?}@/@V@�@�/@��@�j@�j@�@��@z�@(�@1@�m@��@��@t�@"�@o@��@�!11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�ĜA���A��A��A���A�A�%A�1A�
=A�JA�A�
=A�1A��A��TA�(�A�oA���A��!A�O�A�A�5?A��A��`A��
A��A��7A��A�K�A��A��A�I�A�VA�ĜA�I�A���A�`BA�A�A��A���A���A���A���A���A�`BA��yA��uA���A�1A�z�A��\A�+A���A���A���A��TA��/A�r�A�A�x�A�^5A��A�%A�?}A�?}A��/A��DA�JA�x�A���A��#A��A���A��A�%A�z�A���A�O�A~�jA|�+Az(�AxffAw"�Av��Av$�Au;dAs�ArffAq�Aop�Anz�Am�7Am+Al��Al  AkK�Aj�AiƨAh��Ahz�Ag�mAg?}AfĜAfz�Af �AeAe\)Ad�Adr�Ad5?Ad�AdAc�Ac�#Ac��Ac+AbȴAb�jAb�jAb��AbJAa�Aa\)A`��A`jA_�A^��A]��AZVAV�AT��AT�uAS��ARZAQXAPM�AO�AN��AM�AM+AL�AKS�AJ�/AJr�AJ5?AI�;AIl�AH�AG��AFn�ADE�AC�AB�AB�AB^5AB1'AA��AAG�A@��A@�!A@ffA?�
A?33A?
=A>�A>�9A> �A<A�A9��A933A8��A81'A81A7�-A7x�A7A4n�A2�9A1t�A0�HA0jA0 �A/33A.bA-l�A,�A*I�A(��A&��A%A$JA"VA!��A!A!�FA!�PA ��A�7AK�A��A�A&�A�!AA��A��AffAAl�AbNA�9A1A�\A��A�jAVA-A  AȴAA��A��A  A�A7LA
ZA	��A	�PA	��A	�hA	�A�A�\AbNA1'A�;A��AZA�#A33A&�AE�A�
A��A�A j@�o@�5?@�Z@���@�r�@�\@�Z@�C�@�~�@�hs@�j@��@��
@�@��H@�V@�@�/@�Q�@��@�dZ@��@��@䛦@�@�dZ@�33@��@��@��@���@�%@�1'@��@��H@��T@�?}@���@�V@�O�@�  @���@�&�@��;@ʰ!@ǝ�@�^5@ř�@� �@�|�@��@��w@�;d@�
=@�^5@��-@�`B@�1'@���@��H@��@���@�dZ@��@��y@���@�V@��@�{@��T@��^@��7@���@�A�@��@�
=@���@���@�K�@�5?@�hs@��R@��@�o@��!@�=q@��@��@�$�@��@�{@���@��@���@���@�?}@�j@�C�@�
=@��@���@��w@���@�n�@�Ĝ@��+@��T@�@���@�x�@�7L@��`@�bN@�t�@���@���@���@��@��@��#@��-@��@�p�@�O�@�?}@�&�@��@�V@��@�Z@���@�b@�C�@���@�-@��^@�G�@��j@�A�@�1@���@���@�l�@�\)@�\)@�+@�
=@���@�ff@�$�@��@��T@�@��h@�x�@�hs@�X@�O�@�?}@��@���@���@���@��m@�J@��F@��@�t�@�\)@�;d@�
=@�^5@�%@���@�A�@��@�b@�b@�b@�1@�b@�1@�1@��@��m@��;@���@��P@�|�@�l�@�dZ@�\)@�S�@�33@�
=@��!@�{@��-@�V@���@���@��/@���@���@�bN@�  @\)@~v�@}�@}V@|z�@|1@{dZ@y�#@wl�@u�T@u�-@up�@u�@t�/@t��@t�j@t��@t�D@tz�@tj@tZ@t9X@t9X@t9X@t(�@t(�@t�@s�m@sƨ@s�F@s��@sdZ@so@r�!@r-@r�@q�#@q�^@q��@q��@q&�@p��@p �@o�@o|�@o\)@o�@n��@n�y@nȴ@n�+@m��@m/@lj@l1@k��@kt�@k"�@j�@j��@j�!@j^5@i��@h��@hr�@hr�@hA�@hA�@h �@g�w@gl�@f�y@f5?@ep�@d�@dz�@dZ@d(�@c��@cS�@co@c@b�@b�H@b��@b��@b��@b��@b��@b�!@b�!@b��@b��@b��@b�!@b�@a�^@a��@ax�@a&�@`��@`r�@`1'@`  @_�@_;d@^��@^v�@^{@]`B@\�@\�D@\I�@\�@[ƨ@[t�@Z��@Y��@Y��@X�@XbN@Xr�@XQ�@V�y@V@U��@U�h@U�h@Up�@U`B@UO�@U�@U�@T�/@Tz�@Tz�@T(�@S��@S�F@S�F@S�F@S��@S"�@R��@R~�@R�@Q�^@Qhs@Q&�@P�`@PĜ@Pr�@PQ�@PA�@Pb@P  @O��@N��@NE�@N@M@M�@M/@MV@L�/@L�D@Lz�@L9X@K�m@K��@K��@K�@KS�@J�!@J^5@JJ@I�@I�^@IG�@H�`@HbN@H �@H  @G��@G�@G\)@GK�@G;d@G�@G
=@FE�@E��@D�j@D�@C�F@C��@C��@C�@C33@B�H@B��@B~�@BJ@Ahs@A�@@��@@r�@@A�@@b@?�@?�;@?�w@?�@?K�@>�y@>��@>�+@>5?@=�h@=�@<�@<z�@<j@<I�@;��@;ƨ@;t�@;t�@;dZ@;S�@;"�@:�H@9��@9X@9G�@9�@8Ĝ@8�u@8�@8r�@8Q�@8A�@8 �@8  @7�@7�;@7��@7�@7�P@7|�@6��@6��@6��@6��@6ff@6$�@6@5��@5�h@5p�@4��@4��@4z�@3�
@3C�@2�\@1�7@1�@0��@0�9@0r�@01'@/|�@/�@/
=@.��@.�@.��@.ff@.@-@-�h@-p�@-V@,Z@,(�@+�F@+��@+�@+dZ@+C�@+33@+o@*��@*�\@*^5@*M�@*=q@*�@*J@)�@)�#@)��@)�7@)X@(�`@(�u@'�;@'|�@';d@'
=@&�y@&�@&�R@&��@&ff@&V@&E�@&$�@%�@%�T@%�h@%O�@$��@#��@#dZ@#C�@#33@"�@"��@"^5@!��@!�@!�#@!��@!x�@!hs@!X@!G�@!�@ ��@ �9@ ��@ ��@ ��@ �u@ bN@ 1'@ b@��@�@�y@ȴ@ff@V@V@5?@{@@��@O�@��@�j@z�@I�@(�@�@�@�@1@1@ƨ@t�@C�@o@@�H@�!@n�@=q@J@J@��@��@��@��@��@x�@&�@�@�`@Ĝ@��@�@bN@A�@ �@�@��@�;@��@��@\)@�@�y@�@��@�+@ff@5?@{@�@@�@�@p�@?}@/@��@��@I�@9X@9X@9X@9X@��@�
@�F@�@C�@�@��@��@~�@M�@=q@-@J@�#@��@7L@��@��@�`@Ĝ@��@�@bN@Q�@1'@�;@�P@\)@�@��@�y@�@�R@��@��@�+@�+@�+@ff@V@E�@5?@{@�@�@�T@��@��@O�@/@V@��@�/@��@�D@z�@z�@j@I�@(�@�m@��@�@S�@C�@@
�\@
M�@	�#@	��@	x�@	G�@	7L@	7L@	&�@	�@	�@	�@	%@��@�`@�9@ �@��@�P@l�@;d@
=@ȴ@�R@�R@��@��@�+@v�@ff@V@V@E�@5?@@�h@O�@O�@?}@/@V@�@�/@��@�j@�j@�@��@z�@(�@1@�m@��@��@t�@"�@o@��@�!11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�3B�!B�B��Bt�BI�B@�B>wB=qB8RB5?B1'B �B�BPBB��B��B��B��B�B�B�B�B�yB�mB�ZB�;B�5B�5B�5B�/B�B��BŢB�B��B�oB�B|�Bs�Bk�BffBW
BE�B<jB0!B+B&�B�BbBB
�B
�B
�mB
�;B
��B
��B
�}B
�dB
�^B
�LB
�!B
��B
��B
��B
�1B
y�B
jB
`BB
XB
T�B
P�B
J�B
B�B
;dB
2-B
)�B
$�B
�B
�B
�B
�B
{B
bB
JB
1B
B
B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�yB	�yB	�mB	�`B	�NB	�NB	�NB	�HB	�/B	�B	�B	�B	��B	��B	ŢB	�qB	�B	��B	�\B	�JB	�+B	�B	{�B	v�B	q�B	m�B	iyB	ffB	bNB	]/B	[#B	YB	W
B	T�B	Q�B	L�B	G�B	A�B	5?B	2-B	0!B	/B	.B	,B	+B	'�B	&�B	$�B	"�B	 �B	�B	�B	�B	�B	�B	JB	B��B��B��B��B��B��B�B�`B�/B�B��B��B��B��BȴBĜB��B�^B�9B�B��B��B��B��B��B��B��B��B�{B�oB�bB�PB�DB�7B�+B�B� B{�By�Bw�Bs�Bp�Bm�BjBgmBffBdZBcTBbNB`BB^5B\)B[#BYBXBVBT�BS�BQ�BQ�BQ�BP�BO�BO�BN�BL�BK�BH�BE�BD�BC�BB�BA�B@�B?}B=qB;dB8RB6FB5?B33B2-B1'B1'B0!B0!B/B/B/B/B.B.B.B.B-B.B-B-B,B,B,B,B,B,B,B+B)�B(�B(�B(�B'�B'�B&�B%�B%�B$�B#�B!�B!�B"�B"�B!�B$�B$�B$�B%�B$�B&�B'�B'�B&�B'�B'�B'�B(�B'�B(�B-B.B/B/B0!B0!B1'B1'B1'B2-B2-B2-B33B49B49B49B9XB:^B;dB=qB>wBE�BK�BO�BP�BQ�BR�BR�BQ�BQ�BR�BR�BR�BR�BR�BS�BVBZB[#B[#B^5Be`BiyBiyBq�B|�B� B� B�B�B�B�B�B�DB�bB�hB�hB�uB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�-B�9B�?B�FB�LB�FB�LB�RB�^B�jB�wB�}B��B��B��BBBÖBÖBÖBĜBŢBƨBƨB��B�
B�B�B�B�B�B�B��B	B	B	B	%B	%B	+B	+B	+B	+B	+B	+B	1B	1B	1B		7B	JB	PB	\B	bB	oB	{B	�B	�B	#�B	.B	1'B	6FB	6FB	7LB	7LB	7LB	9XB	;dB	?}B	A�B	B�B	C�B	B�B	D�B	I�B	L�B	R�B	[#B	bNB	cTB	dZB	ffB	gmB	hsB	iyB	k�B	k�B	l�B	m�B	n�B	o�B	o�B	o�B	p�B	o�B	p�B	q�B	r�B	r�B	s�B	s�B	u�B	v�B	x�B	y�B	z�B	z�B	z�B	{�B	|�B	~�B	�B	�B	�B	�B	�B	�%B	�+B	�+B	�1B	�JB	�PB	�hB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�'B	�'B	�-B	�-B	�3B	�3B	�3B	�3B	�9B	�9B	�9B	�9B	�9B	�9B	�9B	�FB	�RB	�RB	�XB	�^B	�jB	�wB	�}B	��B	��B	B	ĜB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�
B	�B	�/B	�5B	�;B	�;B	�;B	�;B	�;B	�BB	�BB	�HB	�NB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
1B
1B
	7B
DB
JB
JB
JB
JB
PB
PB
VB
VB
\B
bB
hB
oB
oB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
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
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
+B
-B
-B
.B
.B
.B
/B
0!B
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
5?B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
<jB
<jB
=qB
=qB
=qB
=qB
>wB
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
B�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
F�B
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
G�B
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
J�B
K�B
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
N�B
N�B
O�B
O�B
O�B
P�B
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
R�B
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
[#B
[#B
[#B
[#B
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
^5B
^5B
^5B
^5B
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
bNB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
ffB
gmB
gmB
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
iyB
iyB
iyB
jB
k�B
k�B
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
m�B
m�B
m�B
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
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�B�TB�9B�B�B�9B�9B�9B�B�TB�9B�nB��B�'B�;B��B{BK�BA�B?�B>wB8�B6FB33B"hB�B�BYB��B��B�LB�ZB�B�B�B�]B�B��B��BߤB�OB�OB�jB��B�#B�BǔB�qB�5B�FB�9B~(BuBl�BhsBYBF�B=�B0�B+�B(�B�BTBB
�B
�wB
��B
�B
��B
�JB
��B
��B
��B
��B
�vB
�XB
��B
��B
��B
|jB
lqB
a�B
X�B
U�B
R:B
L�B
D3B
=B
4B
+B
%�B
 BB
�B
qB
yB
�B
NB
B
�B
�B
�B	��B	�JB	�XB	�LB	�?B	�3B	�!B	��B	�B	�B	�B	�B	�B	�
B	��B	�NB	�hB	�B	��B	��B	�kB	خB	��B	�&B	�PB	ǮB	�oB	� B	�EB	�HB	��B	��B	�[B	}<B	w�B	r�B	n}B	jB	gRB	c�B	]�B	[�B	YeB	W�B	U�B	S@B	N"B	I�B	C�B	6`B	2�B	0oB	/�B	.}B	,�B	+�B	(sB	'8B	%`B	#�B	!bB	B	�B	CB	�B	�B	�B	�B��B��B�*B�XB��B�B�B�BބB��BյBөB� B�<B��BƨB��B��B��B�!B�_B��B�OB��B��B�)B��B��B��B�[B��B�B��B�=B��B��B��B|�Bz�ByrBu�Bq�BoOBk�Bh�BgBd�BdBc�BaHB^�B]IB\BY�BX�BV�BU�BT{BRBR:BR�BQ4BPbBPHBOBBM�BM�BKxBFYBESBC�BC�BB'B@�B@OB?�B>wB9>B7�B6zB4�B3�B2�B1�B0�B0�B/�B/�B/iB/iB.}B.�B.�B.�B-�B.cB-wB-�B,�B,�B,�B,=B,=B,=B,=B+�B,B*�B)�B)_B(�B(�B'�B&�B'B%�B%FB$B$B#�B#�B#�B%�B%�B%�B&�B&LB'�B(XB(>B'�B(XB(sB(�B)yB(�B*0B-�B.cB/iB/iB0oB0oB1[B1AB1[B2GB2aB2�B3�B4�B5%B5�B9�B:�B<PB>]B@OBGBL�BP.BQ4BQ�BR�BSBRBQ�BSBS&BS&BS&BSuBT�BV�BZQB[�B\B_�Bf2Bi�Bj�Br�B}VB�4B�4B�;B�;B�aB��B��B��B�bB��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�IB�-B�FB�8B�_B�kB�cB�AB�aB�nB�ZB�`B�LB�`B�fB��B��B��B��B�}B��B��B��BªBªBðBÖBðBĶBżB��B�_B�0B�_B�B�B�B��B��B�3B��B	UB	aB	9B	?B	%B	+B	+B	+B	+B	+B	EB	1B	1B	KB		lB	JB	jB	vB	}B	�B	�B	�B	�B	$ZB	.IB	1�B	6FB	6FB	7fB	7fB	7�B	9�B	;�B	?�B	A�B	B�B	C�B	B�B	D�B	J#B	M�B	S�B	[�B	bhB	cTB	dtB	f�B	g�B	h�B	iyB	k�B	k�B	l�B	m�B	n�B	o�B	o�B	o�B	p�B	o�B	p�B	q�B	r�B	r�B	s�B	s�B	u�B	v�B	x�B	y�B	z�B	z�B	z�B	|B	}"B	.B	�;B	�-B	�-B	�B	�9B	�%B	�EB	�_B	�fB	�dB	��B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�&B	�2B	�$B	��B	�B	�B	�CB	�!B	�;B	�'B	�AB	�B	�GB	�3B	�3B	�MB	�3B	�9B	�B	�9B	�B	�TB	�9B	�nB	�FB	�lB	�lB	�rB	��B	��B	��B	�}B	��B	��B	ªB	ĜB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�4B	�B	�&B	�
B	�$B	�?B	׍B	�QB	�IB	�5B	�;B	�;B	�;B	�VB	�VB	�BB	�\B	�HB	�NB	�hB	�nB	�nB	�ZB	�tB	�tB	�B	�fB	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	��B
  B
 B
 B
;B
;B
'B
3B
B
B
9B
B
B
?B
?B
?B
YB
�B
�B
	lB
^B
JB
dB
JB
dB
jB
PB
pB
�B
vB
bB
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
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
!�B
!�B
"�B
"�B
"�B
"�B
#B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
'B
'�B
'�B
($B
)B
)DB
+6B
-)B
-B
./B
./B
.IB
/OB
0;B
1B
1AB
1'B
1AB
1'B
2GB
2GB
33B
33B
3hB
3hB
5ZB
5tB
6`B
6FB
6FB
7LB
7LB
7fB
7fB
7LB
8lB
8RB
8RB
8lB
8RB
8RB
9>B
9rB
9rB
9rB
:�B
:�B
:�B
<jB
<jB
=�B
=�B
=qB
=qB
>]B
>�B
>wB
>�B
>wB
?}B
?}B
?�B
?�B
@�B
@�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
F�B
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
G�B
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
J�B
K�B
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
N�B
N�B
O�B
O�B
O�B
Q B
Q B
P�B
P�B
P�B
Q B
RB
Q�B
Q�B
RB
RB
Q�B
R�B
R�B
SB
SB
TB
S�B
TB
S�B
S�B
TB
S�B
T�B
UB
UB
VB
VB
VB
VB
VB
W
B
W
B
W$B
W$B
W�B
X+B
X+B
XB
X+B
Y1B
Y1B
ZB
ZB
ZB
ZB
ZB
[=B
[=B
[#B
[#B
\CB
\)B
\)B
]/B
]IB
]/B
]/B
]IB
]IB
]IB
]IB
^OB
^B
^B
^OB
^5B
_;B
_;B
_VB
_;B
_VB
`\B
`BB
`BB
aHB
a-B
aHB
aHB
aHB
a-B
abB
a-B
a-B
aHB
b4B
bNB
b4B
bhB
bNB
bNB
bNB
bNB
bhB
bhB
cnB
cTB
cTB
cTB
dZB
dtB
dZB
d@B
d@B
dtB
dZB
e`B
ezB
ezB
f�B
f�B
f�B
f�B
g�B
g�B
hsB
hsB
iyB
i_B
iyB
iyB
i_B
iyB
iyB
iyB
iyB
iyB
i�B
i�B
j�B
k�B
k�B
l�B
l�B
l�B
m�B
mwB
mwB
m�B
m�B
mwB
m�B
m�B
mwB
mwB
m�B
m�B
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
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.08(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201805090033222018050900332220180509003322202211182134332022111821343320221118213433201806041923582018060419235820180604192358  JA  ARFMdecpA19c                                                                20180429003519  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180428153629  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180428153631  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180428153631  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180428153632  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180428153632  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180428153632  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180428153632  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180428153632  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180428153632                      G�O�G�O�G�O�                JA  ARUP                                                                        20180428155648                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180428153549  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20180508153322  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180508153322  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604102358  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171536                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123433  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                