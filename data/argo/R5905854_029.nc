CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:49:39Z creation;2022-06-04T17:49:39Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p\   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tH   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �p   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �x   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220604174939  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @��\�v�I1   @��\��A@-0��
=q�c���F1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�33@�  A��A   A@  A`  A���A�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BY��B^  Bh  Bp  Bz  B�ffB�  B���B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�fC  C   C"�C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�@\A�HA!G�AAG�AaG�A�p�A���A�p�A���A���AУ�A��A��B Q�BQ�BQ�BQ�B Q�B(Q�B0Q�B8Q�B@Q�BHQ�BPQ�BY�B^Q�BhQ�BpQ�BzQ�B��\B�(�B���B�(�B�(�B�(�B���B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B��\B���B�(�B�(�C {C{C{C{C{C
{C{C{C{C{C{C{C{C{C��C{C {C".C$.C&{C({C*{C,{C.{C0{C2{C4{C6{C8{C:{C<{C>{C@{CB{CD{CF{CH{CJ{CL{CN{CP{CR{CT.CV{CX{CZ{C\{C^{C`{Cb{Cd{Cf{Ch{Cj{Cl{Cn{Cp{Cr{Ct{Cv{Cx{Cz{C|{C~{C�
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
=C�
C�
C�
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
=C�
C�
C�
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
=D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DI�DI��DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�Dp��Dq�DrDr�DsDs�DtDt�DuDu�DvDv�DwDw�DxDx�DyDy�DzDz�D{D{�D|D|�D}D}�D~D~�DD�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��\D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D�D�D��D�B�DÂ�D�D��D�B�DĂ�D�D��D�B�Dł�D�D��D�B�DƂ�D�D��D�B�Dǂ�D�D��D�B�DȂ�D�D��D�B�Dɂ�D�D��D�B�Dʂ�D�D��D�B�D˂�D�D��D�B�D̂�D�D��D�B�D͂�D�D��D�B�D΂�D�D��D�B�Dς�D�D��D�B�DЂ�D�D��D�B�Dт�D�D��D�B�D҂�D�D��D�B�Dӂ�D�D��D�B�DԂ�D�D��D�B�DՂ�D�D��D�B�Dւ�D�D��D�B�Dׂ�D�D��D�B�D؂�D�D��D�B�Dق�D�D��D�B�Dڂ�D�D��D�B�Dۂ�D�D��D�B�D܂�D�D��D�B�D݂�D�D��D�B�Dނ�D�D��D�B�D߂�D�D��D�B�D���D�D��D�B�DႏD�D��D�B�D₏D�D��D�B�DわD�D��D�B�D䂏D�D��D�B�D傏D�D��D�B�D悏D�D��D�B�D炏D�D��D�B�D肏D�D��D�B�D邏D�D��D�B�DꂏD�D��D�B�D낏D�D��D�B�D삏D�D��D�B�D킏D�D��D�B�DD�D��D�B�DD�D��D�B�D���D�D��D�B�D�D�D��D�B�D�D�D��D�?\D�D�D��D�B�D�D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aݴ�Aݯ�A��UA��zA��pA��A�҉A�ܒA��2A�ݘA���A� 4A�=�A�EmA�=qA�>�A�S[Aކ�A��HA��iA��A��A�oA�ÖA�SA�z�A�R�A�hA�zA�xA�(�A�IAͶzA��A���A�O�Aë�A�k�A���A���A���A��+A�rGA��~A�A��!A�v�A�I�A�
	A��WA�Y�A�6�A���A��A�~�A��A�;dA���A��uA��.A�e�A��A���A��A�e,A�uA���A��A�/A��dA��A���A�!�A�m)A��^A���A�)�A�OvA��A�YA�1�A]�AyXyAu�Ao�CAk1�Ag&�AdF�Aa�OA\�AVI�ATffAP�&AM��AK-wAI��AG�AC�bA@�5A?�$A=�BA:�MA8!�A5��A3�BA1�DA/�KA/%FA.e�A-�IA-&�A,�~A,�A)��A(B�A'��A%��A#�A"ߤA!ffA!8�A!S&A��A��A��A�A��Aa�AR�A�AffA;�AS&Ap�A`�A��AYAYKA=An/A�A�UA��An�A'RAJ�A�-A-A��Ap;A?}A��Au�A4nA�EAI�A�AAbNA��At�AJAA8�A�
A0UAa|A}�A�A'RA�A�A��A�PA�AC-AxA
��A
�A
QA	�A	��A	PHA�ZA��Az�AOA�NA}VAA�6AqvA9XA�A��A�zAzxA�AcAA�A|�A>�A��A��A�A�4A=A �A �@A ��A �@��@��@��Y@�k�@���@��N@�4�@���@��T@��@�C�@��o@�j�@��[@�@�@��@�W?@�R�@�!�@��@�x�@��@�u@�ݘ@��@�-�@��a@�:�@�@��@�o@�9@��@���@鹌@�@@�]d@��d@�@�@��6@��`@�V�@�L0@���@�q@���@▼@�M�@��@��D@��Z@�S�@���@��@���@ݭC@݉7@�a�@�)_@��"@��m@�7�@�ϫ@ۏ�@�e,@��P@��E@ڏ\@� �@���@وf@ا�@�6z@��@��U@�l�@՛=@���@ԁo@�bN@�?@�1�@��;@ӳ�@�;d@�͟@҆Y@�'R@���@�a@�;@мj@�GE@ϰ�@�l�@�S�@�6z@�@���@γh@�_@�W�@�!@�j@���@̄�@�J@�hs@�ی@ʅ�@�E�@��
@ȸR@�~(@��@��3@Ǚ�@��@�l�@�	�@ŀ4@ģ@��@�5�@�@�y>@�s�@��@�.I@��@�p;@���@�Vm@� \@��U@�~�@��@�q@�$@���@��$@�f�@��@��@�m]@�ߤ@��@�8�@��m@���@��N@��C@�o@���@�7@��@@��p@��\@�|�@�Z�@�7�@���@�@�8�@�~�@�G�@�s�@��@��@��@���@��{@��M@���@��b@�w�@��@��*@�33@���@�\�@��@�e�@�?}@�u%@��m@���@��	@��@��h@�-@��~@�e�@�%F@���@�2�@�-@�&�@�$@��F@�^�@�S&@�F@���@�K^@���@�F@���@��O@�(�@��@�s�@�@���@�Ɇ@�<�@��C@�W?@�"�@��@��2@���@�U2@�&�@��r@��d@��a@���@�t�@�?}@���@���@��@�c�@�@�@��@�˒@�u�@�@���@��u@�h�@�U2@�Ov@�M�@�M@���@���@��@��@��@��z@�L0@��@��@���@���@�W?@�.I@���@�l�@��@���@�~�@�]�@�c�@�:�@��K@���@�u%@�?�@��@��q@���@�H�@��,@�r�@� �@��+@��@���@��@�<6@��P@��.@�U2@�{@��@�k�@�@�͟@�bN@��@��z@���@�U�@���@��'@��}@�@�@��^@�p�@���@��b@��o@�Z@��@��@��@��:@�q@���@���@��@�;�@�T�@�G�@�:�@�C@��/@�xl@�S�@�J@���@��=@�{J@�p�@�9�@���@��U@��}@���@�q�@�{@��@�0U@��@��D@���@��k@�T�@�4@��@��1@�PH@�@���@�A @� \@�
=@��@��I@���@�Xy@�Q@�_@�4@~�@~8�@}hs@|/�@{qv@{S@z��@z �@y��@yo @y+@x��@x��@x��@x'R@w�g@w��@wl�@vں@v��@va|@v+k@u��@uF@t[�@s��@r�@r	@q�~@q�@p֡@p��@p`�@p6@o��@ox@o@O@n͟@n�!@nL0@m�.@m�C@m[W@m�@l֡@l�@k�}@kt�@k6z@j�@jd�@j�@iԕ@i�^@iF@h��@g��@g��@gY@f��@f��@e��@eB�@d�@dz�@dV�@d:�@c�Q@c{J@c>�@b�s@a�9@a5�@a+@`�_@_�r@_�@@_qv@_C@^�2@^��@^8�@]�.@]��@]f�@]+@\ѷ@\V�@[ݘ@[��@[��@[n/@[ i@Z��@Z.�@Y�n@XɆ@X4n@X  @W�[@WY@V��@Vu%@V�@Ue,@U�@T�o@TI�@S�a@Sl�@R�8@RO@Qf�@Q#�@P�/@P�O@P�.@PD�@P'R@O�@O�P@Oj�@OF�@O&@Nߤ@N��@N�\@Nn�@NJ�@N$�@M�@M�@M8�@L��@Ll"@K��@K��@KX�@K1�@J��@Jq�@I�@Ik�@H�@Hy>@H<�@G�[@G�{@Gj�@G=@F�c@FTa@E�@E�@D��@D�@D@C�A@C�0@C�@B��@B��@BkQ@A��@Ae,@A \@@Ĝ@@w�@@Q�@@  @?�:@>��@>�h@>ff@>O@=�N@=T�@=�@<�9@<?�@;خ@;�{@;S�@;"�@:�,@:�F@:�@9��@9X@8�|@8��@8w�@8j@8,=@7��@7�6@7|�@7+@6��@6��@6��@6��@6xl@6_�@6#:@5��@5|@5S&@5�@4��@4��@4c�@4 �@4@3�@3l�@3o@2�B@2�@2($@1�M@0�K@0��@0�Y@0l"@0U2@0@/˒@/o�@/j�@/X�@/.I@.�c@.�,@.v�@.C�@.1�@.J@-ԕ@-��@-�M@-T�@-�@,Ɇ@,��@,oi@,?�@+��@+�q@+|�@+@O@+
=@*��@*l�@*GE@*!�@)��@)�@)k�@)-w@(��@(g8@'�@'K�@&�@&�b@&{�@&GE@%��@%��@%��@%u�@%a�@%7L@%�@$�Y@$Q�@$2�@$~@$M@$b@#��@#��@#S�@#�@"�@"��@"��@"a|@"E�@"	@!�T@!��@!��@!��@!Vm@!N<@!8�@!q@ ��@ �o@ e�@ <�@��@�@j�@o�@Z�@@��@�@~�@\�@@�@;�@.�@@�@�z@X@�|@֡@��@��@Z@~@�r@�A@� @�*@|�@9�@"�@�@ߤ@҉@�h@l�@1�@�@��@rG@<6@&�@!�@�@��@tT@N�@-�@"h@~@�@�g@��@�*@��@�@E9@�8@��@�!@�@}V@\�@�@��@�@�M@^�@0�@��@�@�v@��@�@��@h�@/�@1@��@��@��@j�@;d@�@�@�@�B@��@��@�+@\�@+k@�)@rG@IR@&�@�@Ɇ@��@�@j@A�@!@��@خ@�:@n/@>�@�@��@�m@�x@��@YK@E�@3�@&�@	@�@}�@G�@ \@�@�	@��@�@Xy@>B@�@�@�01111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aݴ�Aݯ�A��UA��zA��pA��A�҉A�ܒA��2A�ݘA���A� 4A�=�A�EmA�=qA�>�A�S[Aކ�A��HA��iA��A��A�oA�ÖA�SA�z�A�R�A�hA�zA�xA�(�A�IAͶzA��A���A�O�Aë�A�k�A���A���A���A��+A�rGA��~A�A��!A�v�A�I�A�
	A��WA�Y�A�6�A���A��A�~�A��A�;dA���A��uA��.A�e�A��A���A��A�e,A�uA���A��A�/A��dA��A���A�!�A�m)A��^A���A�)�A�OvA��A�YA�1�A]�AyXyAu�Ao�CAk1�Ag&�AdF�Aa�OA\�AVI�ATffAP�&AM��AK-wAI��AG�AC�bA@�5A?�$A=�BA:�MA8!�A5��A3�BA1�DA/�KA/%FA.e�A-�IA-&�A,�~A,�A)��A(B�A'��A%��A#�A"ߤA!ffA!8�A!S&A��A��A��A�A��Aa�AR�A�AffA;�AS&Ap�A`�A��AYAYKA=An/A�A�UA��An�A'RAJ�A�-A-A��Ap;A?}A��Au�A4nA�EAI�A�AAbNA��At�AJAA8�A�
A0UAa|A}�A�A'RA�A�A��A�PA�AC-AxA
��A
�A
QA	�A	��A	PHA�ZA��Az�AOA�NA}VAA�6AqvA9XA�A��A�zAzxA�AcAA�A|�A>�A��A��A�A�4A=A �A �@A ��A �@��@��@��Y@�k�@���@��N@�4�@���@��T@��@�C�@��o@�j�@��[@�@�@��@�W?@�R�@�!�@��@�x�@��@�u@�ݘ@��@�-�@��a@�:�@�@��@�o@�9@��@���@鹌@�@@�]d@��d@�@�@��6@��`@�V�@�L0@���@�q@���@▼@�M�@��@��D@��Z@�S�@���@��@���@ݭC@݉7@�a�@�)_@��"@��m@�7�@�ϫ@ۏ�@�e,@��P@��E@ڏ\@� �@���@وf@ا�@�6z@��@��U@�l�@՛=@���@ԁo@�bN@�?@�1�@��;@ӳ�@�;d@�͟@҆Y@�'R@���@�a@�;@мj@�GE@ϰ�@�l�@�S�@�6z@�@���@γh@�_@�W�@�!@�j@���@̄�@�J@�hs@�ی@ʅ�@�E�@��
@ȸR@�~(@��@��3@Ǚ�@��@�l�@�	�@ŀ4@ģ@��@�5�@�@�y>@�s�@��@�.I@��@�p;@���@�Vm@� \@��U@�~�@��@�q@�$@���@��$@�f�@��@��@�m]@�ߤ@��@�8�@��m@���@��N@��C@�o@���@�7@��@@��p@��\@�|�@�Z�@�7�@���@�@�8�@�~�@�G�@�s�@��@��@��@���@��{@��M@���@��b@�w�@��@��*@�33@���@�\�@��@�e�@�?}@�u%@��m@���@��	@��@��h@�-@��~@�e�@�%F@���@�2�@�-@�&�@�$@��F@�^�@�S&@�F@���@�K^@���@�F@���@��O@�(�@��@�s�@�@���@�Ɇ@�<�@��C@�W?@�"�@��@��2@���@�U2@�&�@��r@��d@��a@���@�t�@�?}@���@���@��@�c�@�@�@��@�˒@�u�@�@���@��u@�h�@�U2@�Ov@�M�@�M@���@���@��@��@��@��z@�L0@��@��@���@���@�W?@�.I@���@�l�@��@���@�~�@�]�@�c�@�:�@��K@���@�u%@�?�@��@��q@���@�H�@��,@�r�@� �@��+@��@���@��@�<6@��P@��.@�U2@�{@��@�k�@�@�͟@�bN@��@��z@���@�U�@���@��'@��}@�@�@��^@�p�@���@��b@��o@�Z@��@��@��@��:@�q@���@���@��@�;�@�T�@�G�@�:�@�C@��/@�xl@�S�@�J@���@��=@�{J@�p�@�9�@���@��U@��}@���@�q�@�{@��@�0U@��@��D@���@��k@�T�@�4@��@��1@�PH@�@���@�A @� \@�
=@��@��I@���@�Xy@�Q@�_@�4@~�@~8�@}hs@|/�@{qv@{S@z��@z �@y��@yo @y+@x��@x��@x��@x'R@w�g@w��@wl�@vں@v��@va|@v+k@u��@uF@t[�@s��@r�@r	@q�~@q�@p֡@p��@p`�@p6@o��@ox@o@O@n͟@n�!@nL0@m�.@m�C@m[W@m�@l֡@l�@k�}@kt�@k6z@j�@jd�@j�@iԕ@i�^@iF@h��@g��@g��@gY@f��@f��@e��@eB�@d�@dz�@dV�@d:�@c�Q@c{J@c>�@b�s@a�9@a5�@a+@`�_@_�r@_�@@_qv@_C@^�2@^��@^8�@]�.@]��@]f�@]+@\ѷ@\V�@[ݘ@[��@[��@[n/@[ i@Z��@Z.�@Y�n@XɆ@X4n@X  @W�[@WY@V��@Vu%@V�@Ue,@U�@T�o@TI�@S�a@Sl�@R�8@RO@Qf�@Q#�@P�/@P�O@P�.@PD�@P'R@O�@O�P@Oj�@OF�@O&@Nߤ@N��@N�\@Nn�@NJ�@N$�@M�@M�@M8�@L��@Ll"@K��@K��@KX�@K1�@J��@Jq�@I�@Ik�@H�@Hy>@H<�@G�[@G�{@Gj�@G=@F�c@FTa@E�@E�@D��@D�@D@C�A@C�0@C�@B��@B��@BkQ@A��@Ae,@A \@@Ĝ@@w�@@Q�@@  @?�:@>��@>�h@>ff@>O@=�N@=T�@=�@<�9@<?�@;خ@;�{@;S�@;"�@:�,@:�F@:�@9��@9X@8�|@8��@8w�@8j@8,=@7��@7�6@7|�@7+@6��@6��@6��@6��@6xl@6_�@6#:@5��@5|@5S&@5�@4��@4��@4c�@4 �@4@3�@3l�@3o@2�B@2�@2($@1�M@0�K@0��@0�Y@0l"@0U2@0@/˒@/o�@/j�@/X�@/.I@.�c@.�,@.v�@.C�@.1�@.J@-ԕ@-��@-�M@-T�@-�@,Ɇ@,��@,oi@,?�@+��@+�q@+|�@+@O@+
=@*��@*l�@*GE@*!�@)��@)�@)k�@)-w@(��@(g8@'�@'K�@&�@&�b@&{�@&GE@%��@%��@%��@%u�@%a�@%7L@%�@$�Y@$Q�@$2�@$~@$M@$b@#��@#��@#S�@#�@"�@"��@"��@"a|@"E�@"	@!�T@!��@!��@!��@!Vm@!N<@!8�@!q@ ��@ �o@ e�@ <�@��@�@j�@o�@Z�@@��@�@~�@\�@@�@;�@.�@@�@�z@X@�|@֡@��@��@Z@~@�r@�A@� @�*@|�@9�@"�@�@ߤ@҉@�h@l�@1�@�@��@rG@<6@&�@!�@�@��@tT@N�@-�@"h@~@�@�g@��@�*@��@�@E9@�8@��@�!@�@}V@\�@�@��@�@�M@^�@0�@��@�@�v@��@�@��@h�@/�@1@��@��@��@j�@;d@�@�@�@�B@��@��@�+@\�@+k@�)@rG@IR@&�@�@Ɇ@��@�@j@A�@!@��@خ@�:@n/@>�@�@��@�m@�x@��@YK@E�@3�@&�@	@�@}�@G�@ \@�@�	@��@�@Xy@>B@�@�@�01111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B8B8�B:B:�B<6B<�B<�BCBH�BC�B=�BU�Bv�B�B�,B��B�6B�$B	(XB	abB	}B	��B	�B	��B	�}B	�B	��B	�'B	�xB	ɠB	��B	ԯB	�EB	�QB
FB
<�B
k�B
��B
�B
��B�B5BB�BN�BQ�BT�B��B�gB��B��B��B�aB�B�>B��B�B��B�B�B��B��B��B�WB��B��B�B��Bh�B{�B��Bv�BO(B0;BBB
��B
�_B
�zB
��B
k�B
<B
#nB	��B	��B	��B	��B	e`B	N�B	;�B	+QB	�B�WB�B��B�^B�EB�BΥB��BƨB�-B�-B�3B��B�OB�IB�B�B��B�B�VBÖB��B��B�TB�uB�B��B׍B�vB�iB�DB	�B	\B	�B	B	0B	@ B	B�B	DMB	HfB	X�B	n�B	{JB	�MB	��B	��B	��B	��B	�ZB	��B	�jB	�6B	��B	�$B	��B	��B	�hB	��B	��B	�aB	�|B	�MB	�B	��B	�`B	��B	��B	�dB	�B	��B	��B	��B	�JB	�B	�B	�B	�JB	�>B	�LB	�.B	��B	�9B	��B	�<B	��B	��B	��B	��B	��B	��B	�;B	�B	��B	��B	�MB	��B	�YB	��B	��B	ǔB	ȚB	ȴB	�B	ɠB	�	B	�rB	�)B	̳B	��B	�<B	�<B	οB	��B	�BB	�}B	�4B	��B	�:B	ңB	�B	ҽB	�uB	��B	��B	��B	��B	ևB	�sB	�
B	׍B	רB	�B	�_B	�B	�B	��B	�B	�B	ٴB	ٴB	��B	�B	�B	�QB	چB	ںB	�	B	�WB	یB	�)B	��B	�IB	�dB	�B	��B	޸B	ޞB	�jB	ބB	޸B	�;B	�VB	�pB	߾B	߾B	߾B	�BB	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�4B	�hB	�B	�B	�B	�B	�TB	�B	�B	�B	�FB	�B	��B	�8B	�XB	�RB	�8B	�8B	�RB	�DB	�B	��B	��B	�B	�0B	�B	�eB	�B	�=B	�CB	�wB	�5B	��B	�B	�'B	�B	�vB	��B	�-B	�aB	�|B	�|B	��B	�9B	�9B	�B	��B	�B	�%B	�tB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�.B	�B	�BB	��B	�VB	�<B	��B	��B	�(B	��B	��B	��B	��B	�(B	��B
 �B
�B
 B	�cB	�BB	��B	�}B	�.B	��B	�HB	�}B
 �B
 �B
  B	��B
 B
 �B
B
'B
GB
�B
'B
�B
 �B
 �B
 �B
 iB
  B	�B	��B	�(B	�qB	��B	�PB	�dB	�JB	�B	�0B	�VB	��B	��B	��B	��B
  B
 �B
 �B
 4B
 B
UB
 B
 �B
 �B
 B	��B
 B
 �B
 �B
 �B
�B
[B
-B
�B
-B
B
B
�B
aB
gB
B
�B
�B
B
�B
�B
SB
SB
%B
�B
�B
EB
EB
_B
�B
B
1B
fB
fB
fB
�B
	B
	B
	lB
	�B
	�B
	�B
	�B

=B

�B

�B
B
)B
DB
xB
�B
0B
�B
B
PB
�B
�B
�B
PB
B
VB
�B
�B
�B
B
HB
}B
}B
 B
B
B
NB
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
KB
B
�B
�B
�B
�B
�B
�B
B
�B
�B
~B
/B
B
OB
�B
�B
�B
�B
�B
�B
B
VB
pB
 B
 'B
 \B
 �B
!HB
!-B
!|B
!�B
!�B
!�B
!�B
"4B
"�B
#�B
#�B
$&B
$@B
$&B
$&B
$ZB
$�B
%�B
%`B
%�B
&LB
(sB
)DB
)B
)B
)�B
*�B
*B
*�B
+�B
+kB
,=B
-�B
.IB
.IB
./B
-�B
.�B
/iB
/OB
.�B
.}B
/ B
/B
/B
.�B
/iB
1AB
0�B
1�B
2B
2|B
2�B
2�B
2�B
2|B
1�B
1�B
2GB
2�B
2�B
3hB
3�B
3�B
49B
4TB
4�B
4�B
5tB
6B
6B
6+B
6`B
6�B
6�B
6�B
6�B
7B
7�B
8�B
9$B
9	B
8�B
8lB
8lB
8�B
9	B
9$B
9$B
9XB
9>B
9�B
9�B
9�B
9�B
:B
:*B
:xB
:�B
:�B
;0B
;B
;�B
<B
<jB
<�B
<�B
="B
>(B
?B
?}B
?.B
?}B
?�B
@B
@�B
AB
AoB
A�B
A�B
A�B
A�B
BAB
B[B
B[B
B�B
B�B
BuB
B�B
B�B
B�B
C-B
CGB
CGB
C{B
C�B
C�B
C�B
DB
DgB
D�B
EB
EB
D�B
EB
D�B
E9B
E9B
EmB
E�B
F?B
FtB
F�B
F�B
G_B
GEB
G+B
GzB
G�B
G�B
HfB
H�B
H�B
IB
IlB
J#B
J�B
J�B
K)B
KDB
KDB
KxB
KxB
K�B
K�B
K�B
K�B
LB
LJB
LdB
LdB
L~B
L�B
L�B
L�B
MB
MB
M6B
M�B
N<B
NpB
N�B
N�B
N�B
O(B
OvB
PHB
Q B
Q B
QB
QNB
QNB
Q4B
Q B
Q4B
Q�B
R B
R B
R�B
R�B
S&B
S&B
S@B
TB
TFB
TFB
T{B
T�B
U2B
UMB
U�B
U�B
U�B
U�B
VmB
W
B
W
B
W?B
WYB
W�B
W�B
XB
X_B
X�B
X�B
Y1B
YB
Y1B
YeB
Y�B
Y�B
ZQB
ZQB
Z�B
Z�B
[=B
[#B
[qB
[qB
[�B
[�B
\]B
\�B
\�B
\�B
\�B
\�B
\�B
]/B
]~B
]~B
]�B
]�B
^B
^OB
^jB
^jB
^OB
^OB
^�B
^�B
_B
_!B
_VB
_�B
`'B
`'B
`'B
`\B
`vB
`�B
`�B
aB
aB
aB
a-B
abB
abB
a�B
a�B
a�B
bB
b4B
bNB
bhB
b�B
b�B
b�B
c B
c:B
cTB
c�B
c�B
c�B
c�B
dB
d�B
d�B
d�B
eB
eFB
eFB
e�B
e�B
fB
fLB
f�B
gB
gmB
g�B
g�B
g�B
h
B
hXB
hXB
hXB
hsB
h�B
iB
i�B
i�B
i�B
i�B
i�B
i�B
jB
jeB
j�B
j�B
j�B
kB
kkB
k�B
k�B
k�B
lB
lWB
l�B
l�B
mCB
m)B
m)B
mB
m)B
m)B
mCB
m�B
nIB
n�B
n�B
n�B
o B
oB
o5B
o�B
oiB
o�B
o�B
o�B
o�B
o�B
o�B
pB
poB
p�B
p�B
p�B
qB
qAB
qvB
q�B
q�B
q�B
q�B
q�B
q�B
rB
r-B
rGB
rGB
raB
r�B
r�B
r�B
sB
s3B
sMB
shB
shB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
tTB
t�B
t�B
t�B
uB
u?B
utB
u�B
u�B
u�B
u�B
u�B
u�B
v+B
v`B
vzB
v�B
v�B
v�B
v�B
wB
v�B
v�B
wfB
w�B
w�B
xB
x8B
x8B
xRB
x�B
x�B
x�B
y$B
y$B
y$B
y>B
yXB
yXB
y�B
y�B
y�B
y�B
y�B
zB
zDB
z�B
z�B
z�B
z�B
z�B
{B
{dB
{B
{�B
{�B
|B
|6B
|jB
|�B
|�B
|�B
|�B
}B
}"B
}"B
}"B
}VB
}�B
}�B
~B
~B
~BB
~]B
~�B
~�B
~�B
~�B
HB
c1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B8B8�B:B:�B<6B<�B<�BCBH�BC�B=�BU�Bv�B�B�,B��B�6B�$B	(XB	abB	}B	��B	�B	��B	�}B	�B	��B	�'B	�xB	ɠB	��B	ԯB	�EB	�QB
FB
<�B
k�B
��B
�B
��B�B5BB�BN�BQ�BT�B��B�gB��B��B��B�aB�B�>B��B�B��B�B�B��B��B��B�WB��B��B�B��Bh�B{�B��Bv�BO(B0;BBB
��B
�_B
�zB
��B
k�B
<B
#nB	��B	��B	��B	��B	e`B	N�B	;�B	+QB	�B�WB�B��B�^B�EB�BΥB��BƨB�-B�-B�3B��B�OB�IB�B�B��B�B�VBÖB��B��B�TB�uB�B��B׍B�vB�iB�DB	�B	\B	�B	B	0B	@ B	B�B	DMB	HfB	X�B	n�B	{JB	�MB	��B	��B	��B	��B	�ZB	��B	�jB	�6B	��B	�$B	��B	��B	�hB	��B	��B	�aB	�|B	�MB	�B	��B	�`B	��B	��B	�dB	�B	��B	��B	��B	�JB	�B	�B	�B	�JB	�>B	�LB	�.B	��B	�9B	��B	�<B	��B	��B	��B	��B	��B	��B	�;B	�B	��B	��B	�MB	��B	�YB	��B	��B	ǔB	ȚB	ȴB	�B	ɠB	�	B	�rB	�)B	̳B	��B	�<B	�<B	οB	��B	�BB	�}B	�4B	��B	�:B	ңB	�B	ҽB	�uB	��B	��B	��B	��B	ևB	�sB	�
B	׍B	רB	�B	�_B	�B	�B	��B	�B	�B	ٴB	ٴB	��B	�B	�B	�QB	چB	ںB	�	B	�WB	یB	�)B	��B	�IB	�dB	�B	��B	޸B	ޞB	�jB	ބB	޸B	�;B	�VB	�pB	߾B	߾B	߾B	�BB	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�4B	�hB	�B	�B	�B	�B	�TB	�B	�B	�B	�FB	�B	��B	�8B	�XB	�RB	�8B	�8B	�RB	�DB	�B	��B	��B	�B	�0B	�B	�eB	�B	�=B	�CB	�wB	�5B	��B	�B	�'B	�B	�vB	��B	�-B	�aB	�|B	�|B	��B	�9B	�9B	�B	��B	�B	�%B	�tB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�.B	�B	�BB	��B	�VB	�<B	��B	��B	�(B	��B	��B	��B	��B	�(B	��B
 �B
�B
 B	�cB	�BB	��B	�}B	�.B	��B	�HB	�}B
 �B
 �B
  B	��B
 B
 �B
B
'B
GB
�B
'B
�B
 �B
 �B
 �B
 iB
  B	�B	��B	�(B	�qB	��B	�PB	�dB	�JB	�B	�0B	�VB	��B	��B	��B	��B
  B
 �B
 �B
 4B
 B
UB
 B
 �B
 �B
 B	��B
 B
 �B
 �B
 �B
�B
[B
-B
�B
-B
B
B
�B
aB
gB
B
�B
�B
B
�B
�B
SB
SB
%B
�B
�B
EB
EB
_B
�B
B
1B
fB
fB
fB
�B
	B
	B
	lB
	�B
	�B
	�B
	�B

=B

�B

�B
B
)B
DB
xB
�B
0B
�B
B
PB
�B
�B
�B
PB
B
VB
�B
�B
�B
B
HB
}B
}B
 B
B
B
NB
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
KB
B
�B
�B
�B
�B
�B
�B
B
�B
�B
~B
/B
B
OB
�B
�B
�B
�B
�B
�B
B
VB
pB
 B
 'B
 \B
 �B
!HB
!-B
!|B
!�B
!�B
!�B
!�B
"4B
"�B
#�B
#�B
$&B
$@B
$&B
$&B
$ZB
$�B
%�B
%`B
%�B
&LB
(sB
)DB
)B
)B
)�B
*�B
*B
*�B
+�B
+kB
,=B
-�B
.IB
.IB
./B
-�B
.�B
/iB
/OB
.�B
.}B
/ B
/B
/B
.�B
/iB
1AB
0�B
1�B
2B
2|B
2�B
2�B
2�B
2|B
1�B
1�B
2GB
2�B
2�B
3hB
3�B
3�B
49B
4TB
4�B
4�B
5tB
6B
6B
6+B
6`B
6�B
6�B
6�B
6�B
7B
7�B
8�B
9$B
9	B
8�B
8lB
8lB
8�B
9	B
9$B
9$B
9XB
9>B
9�B
9�B
9�B
9�B
:B
:*B
:xB
:�B
:�B
;0B
;B
;�B
<B
<jB
<�B
<�B
="B
>(B
?B
?}B
?.B
?}B
?�B
@B
@�B
AB
AoB
A�B
A�B
A�B
A�B
BAB
B[B
B[B
B�B
B�B
BuB
B�B
B�B
B�B
C-B
CGB
CGB
C{B
C�B
C�B
C�B
DB
DgB
D�B
EB
EB
D�B
EB
D�B
E9B
E9B
EmB
E�B
F?B
FtB
F�B
F�B
G_B
GEB
G+B
GzB
G�B
G�B
HfB
H�B
H�B
IB
IlB
J#B
J�B
J�B
K)B
KDB
KDB
KxB
KxB
K�B
K�B
K�B
K�B
LB
LJB
LdB
LdB
L~B
L�B
L�B
L�B
MB
MB
M6B
M�B
N<B
NpB
N�B
N�B
N�B
O(B
OvB
PHB
Q B
Q B
QB
QNB
QNB
Q4B
Q B
Q4B
Q�B
R B
R B
R�B
R�B
S&B
S&B
S@B
TB
TFB
TFB
T{B
T�B
U2B
UMB
U�B
U�B
U�B
U�B
VmB
W
B
W
B
W?B
WYB
W�B
W�B
XB
X_B
X�B
X�B
Y1B
YB
Y1B
YeB
Y�B
Y�B
ZQB
ZQB
Z�B
Z�B
[=B
[#B
[qB
[qB
[�B
[�B
\]B
\�B
\�B
\�B
\�B
\�B
\�B
]/B
]~B
]~B
]�B
]�B
^B
^OB
^jB
^jB
^OB
^OB
^�B
^�B
_B
_!B
_VB
_�B
`'B
`'B
`'B
`\B
`vB
`�B
`�B
aB
aB
aB
a-B
abB
abB
a�B
a�B
a�B
bB
b4B
bNB
bhB
b�B
b�B
b�B
c B
c:B
cTB
c�B
c�B
c�B
c�B
dB
d�B
d�B
d�B
eB
eFB
eFB
e�B
e�B
fB
fLB
f�B
gB
gmB
g�B
g�B
g�B
h
B
hXB
hXB
hXB
hsB
h�B
iB
i�B
i�B
i�B
i�B
i�B
i�B
jB
jeB
j�B
j�B
j�B
kB
kkB
k�B
k�B
k�B
lB
lWB
l�B
l�B
mCB
m)B
m)B
mB
m)B
m)B
mCB
m�B
nIB
n�B
n�B
n�B
o B
oB
o5B
o�B
oiB
o�B
o�B
o�B
o�B
o�B
o�B
pB
poB
p�B
p�B
p�B
qB
qAB
qvB
q�B
q�B
q�B
q�B
q�B
q�B
rB
r-B
rGB
rGB
raB
r�B
r�B
r�B
sB
s3B
sMB
shB
shB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
tTB
t�B
t�B
t�B
uB
u?B
utB
u�B
u�B
u�B
u�B
u�B
u�B
v+B
v`B
vzB
v�B
v�B
v�B
v�B
wB
v�B
v�B
wfB
w�B
w�B
xB
x8B
x8B
xRB
x�B
x�B
x�B
y$B
y$B
y$B
y>B
yXB
yXB
y�B
y�B
y�B
y�B
y�B
zB
zDB
z�B
z�B
z�B
z�B
z�B
{B
{dB
{B
{�B
{�B
|B
|6B
|jB
|�B
|�B
|�B
|�B
}B
}"B
}"B
}"B
}VB
}�B
}�B
~B
~B
~BB
~]B
~�B
~�B
~�B
~�B
HB
c1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104946  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174939  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174939  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174939                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024946  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024946  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                