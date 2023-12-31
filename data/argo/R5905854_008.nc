CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:45:49Z creation;2022-06-04T17:45:49Z conversion to V3.1      
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
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ph   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tT   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Έ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �h   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �h   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604174549  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @ظ��?V1   @ظ�(��m@.�hr� ��dDj~��#1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�33@�  A��A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B���B�  B���B�  B�  B�ffB�  B�ffB���B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�CL�C��C�fC   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<L�C=�fC@  CB  CD  CE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&fD&� D'  D'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds�fDtfDt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�R@�@\A�HA!G�AAG�A_�A���A���A���A���A���AУ�A��A��B Q�BQ�BQ�BQ�B Q�B(Q�B0Q�B8Q�B@Q�BHQ�BPQ�BXQ�B`Q�BhQ�BpQ�BxQ�B�(�B�\)B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B���B�(�B�(�B�(�B�(�B���B�(�B���B�(�B�(�B�\B�(�B�\B�B�(�C {C{C{C{C{C
{C{C{C{C{C{C{C.CaHC�C��C {C"{C${C&{C({C*{C,{C.{C0{C2{C4{C6{C8{C:{C<aHC=��C@{CB{CD{CE��CH{CJ{CL{CN{CP{CR{CT{CV{CX{CZ{C\{C^{C`{Cb{Cd{Cf{Ch{Cj{Cl{Cn{Cp{Cr{Ct{Cv{Cx{Cz{C|{C~{C�
=C�
=C�
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
=C��pC�
=C�
=C�
=C�
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
=C��pC�
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
C�
=C�
=C�
=C�
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
=C�
C�
C�
=C�
=C�
=C�
=C�
=D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&�D&�D'D'��D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs��Dt�Dt�DuDu�DvDv�DwDw�DxDx�DyDy�DzDz�D{D{�D|D|�D}D}�D~D~�DD�D��D�B�D���D�D��D�B�D���D�D��D�?\D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�E�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D�D�D��D�B�DÂ�D�D��D�B�DĂ�D�D��D�B�Dł�D�D��D�B�DƂ�D�D��D�B�Dǂ�D�D��D�B�DȂ�D�D��D�B�Dɂ�D�D��D�B�Dʂ�D�D��D�B�D˂�D�D��D�B�D̂�D�D��D�B�D͂�D�D��D�B�D΂�D�D��D�B�Dς�D�D��D�B�DЂ�D�D��D�B�Dт�D�D��D�B�D҂�D�D��D�B�Dӂ�D�D��D�B�DԂ�D�D��D�B�DՂ�D�D��D�B�Dւ�D�D��D�B�Dׂ�D�D��D�B�D؂�D�D��D�B�Dق�D�D��D�B�Dڂ�D�D��D�B�Dۂ�D�D��D�B�D܂�D�D��D�B�D݂�D�D��D�B�Dނ�D�D��D�B�D߂�D�D��D�B�D���D�D��D�B�DႏD�D��D�B�D₏D�D��D�B�DわD�D��D�B�D䂏D�D��D�B�D傏D�D��D�B�D悏D�D��D�B�D炏D�D��D�B�D肏D�D��D�B�D邏D�D��D�B�DꂏD�D��D�B�D낏D�D��D�B�D삏D�D��D�B�D킏D�D��D�B�DD�D��D�B�DD�D��D�B�D���D�D��D�B�D�D�D��D�B�D�D�D��D�B�D�D�D��D�B�D�D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��\D�B�D���D�D��D�B�D���D�D��\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aз�Aл�Aм6Aл�Aн<Aн<Aм�Aн<Aи�Aж�Aж�AЪ0AК�AЖ�AГ@AЛ=AКAХFA�q�AІ%A�y>A�c A�@A�+6A�%A�$A�#:A�"hA�"4A�"�A�!-A� �A� 'A� 'A� \A��A��A�A��A��A�!A�~A�=A��A�A�
=A��A�zDA���A���AA�U�A�4nA�O�A���A��A���A�kA���A��A��A�F�A�ffA���A��A�%�A�v�A���A�b�A�˒A��A��A�u%A��kA��A���A��dA�ٴA��6A��A�ںA���A�jA���A���A�2aA��YA�.}A���A�.�A��BA�ĜA�)*A�AA{�7AzB[AyY�Av� AtQ�AqYAlAi]�AghsAeԕAcn/Ab!�AaL0A_��AZ��AX��AU��ASC�AN��AN�:AMB[AK(�AG��AE��AB�sA@�QA?�;A>?}A<�A;H�A9�*A8��A81�A7:�A4��A3�A1�A/ѷA.��A.A-g�A+ϫA+u�A+a|A+ѷA)�	A(��A)ߤA){JA(�{A(^5A(=A'�A'6A&�A%�eA$MA"��A!��A!�A |�A�Al�Ae�A�A�hA�AE9AoARTA��A��A�=AM�A�7A��A5?A��AH�AȴA��A��Ak�A<�A1A��A��A7�A�-AxAo A9XA��A��A��AS&A�>A�uAFtA
��A
��A
MjA
�A	�vA	�"A	�Ae,A)�A	A��AoA�=A�QA��A�1A��A�'A�@A�AxlA�A�~A iA��A��A��AaA�UA�AW�A33A�A ��A j�@�m]@�	@��A@���@��	@���@���@�|�@�m�@��@�2a@�!�@��6@�0U@�	l@��@�bN@웦@�t�@�ѷ@�O@�4@�v�@�Y@��@��B@�q@�t�@�:*@�/�@���@��@���@�J#@ެ@��;@�q@ܻ�@�w�@��+@ۂ�@�ߤ@�V�@���@�?�@�@ֶ�@�w�@�R�@՜�@Կ�@�U2@ӈf@�w�@�ԕ@�t�@�'�@а�@�N�@��;@ϕ�@��@�z�@��
@�!-@��U@�'R@�˒@�,�@ʍ�@�@ɿH@ɶF@�a�@�@�֡@��@�.I@���@�y>@ż@�
=@ć�@�v�@�PH@��]@�'�@�<�@��d@�&@���@���@�_�@��[@�<6@���@��@�_�@���@�a�@�L�@�@�ѷ@��@���@�Ov@��@�s�@�8@��@���@�V@�@���@�8@��Y@�D�@�b@�˒@���@���@�c�@�:�@�!�@��@���@��@�O@��{@�P�@�&�@��@���@��+@�t�@�Q�@��@�e�@�خ@���@�~�@�dZ@��j@�9X@���@���@�p�@�Vm@�8�@��@�ی@��1@�6@���@�5�@��s@�2�@��@��T@���@��$@�m]@�7L@��M@��@��@�{�@�D�@��@��:@�hs@�^�@�;d@���@��.@�q@�� @�9�@��@���@��@�'R@��@�x@�%F@�u�@�$�@�J@��Q@�Q�@��v@���@�-�@��o@��@���@�G�@��H@���@�h�@�/�@��t@�;d@��@�ȴ@�J�@��@�Q�@�!�@�C@��@�H@��@�S&@��/@���@��4@�z@�.�@�_@��a@��@�k�@��@���@�W�@��#@���@��@��L@�M@��@��C@���@�y�@�X@�$t@���@���@��.@�(�@���@���@�|�@�N<@�?}@��@��@��@���@��2@�ی@���@��@�V�@�G@���@�!�@�ȴ@��1@�u�@�H@�� @��@���@�s�@�S�@�?}@��@�ߤ@��L@�~(@�I�@��@��m@��@@�J�@��]@��m@���@��}@��m@���@���@�0U@���@�Q�@�@���@���@���@�l�@��*@�Dg@��@�ѷ@��@�i�@�6�@�	@��&@�{J@�S&@�L�@�:�@�@��@�;�@�{@��D@��
@���@��@��@��@��@���@��1@�`�@�7@��P@�=@��P@��'@��\@��@���@���@��@�<6@�	l@��P@��@��X@�B[@�A@x@�@~�m@~��@~L0@~-@}�.@|��@|Ft@{�f@{_p@{o@z��@z�h@z��@zH�@y�#@yL�@x��@x�@x�u@x�@wRT@w8@v�]@v�@uϫ@uc�@u%@tFt@s�A@sqv@s6z@r��@q�o@q<6@p�@p�@o��@oY@n��@n$�@m��@m;@l��@l��@l�.@lj@lU2@k�@@j�"@jJ�@j.�@j#:@j4@i�@iX@i	l@h�@h��@h-�@g1�@fi�@e�#@e�@e#�@d�p@dj@d%�@c��@c]�@cMj@c1�@c$t@c"�@c@b�\@b;�@a�9@a[W@a�@`�[@`��@`�.@``�@`7�@`9X@` �@_�@_��@_��@_,�@^�H@^�1@^d�@^u@]-w@\Ɇ@\U2@\A�@\  @[�a@[��@[8@Z�B@Z�!@Z�X@Zp;@Z$�@Y��@Y\�@X��@W�m@Wl�@WS�@V�@VJ�@U�H@Uq@T��@TPH@TG@S�f@Sn/@R��@Q�@Q��@Q��@Q?}@P�)@P�$@P��@P	�@Oخ@OZ�@N�@NZ�@M�@M?}@M@L�9@Lh�@LH@L'R@K��@K{J@K6z@J��@J�'@J�,@J��@JE�@J�@I�.@I�o@I�X@H��@H'R@G�@G��@GY@F�'@Fu%@F$�@Fu@E��@Ek�@E�@D��@D_@DI�@D*�@Dx@C�@C�w@C�:@C8@Co@B�@B#:@A�@A�7@A@@@�e@@9X@?�@?9�@>�@>�m@>��@>Q@=�@=�7@<�@<��@<S�@;�@;��@;a@;�@:ߤ@:��@:��@:i�@:.�@:@9�-@9�@8�@8U2@7��@71�@6҉@6��@6J�@65?@6+k@6�@6e@6e@5�3@5L�@5�@4�5@4u�@3�Q@3��@3dZ@38@2�m@2�F@2s�@2R�@2GE@2�@1��@1j@1A @1&�@1�@0�K@0�[@0��@0q@01'@/��@/b�@/C�@/.I@.��@.�L@.Ta@.!�@-�>@-�h@-4@-�@,ی@,��@,/�@+خ@+)_@*�@*�y@*�H@*�@*~�@)�)@)�@)��@)c@)�@)��@)c�@)�@(��@(��@(`�@(A�@'��@'>�@'o@'@&��@&�@&+k@%��@%`B@%%F@$��@$<�@#�W@#�;@#��@#$t@"�r@"C�@"O@!��@!�>@!��@!�#@!��@!��@!-w@ �p@ �@ �@ w�@ �@�F@>�@/�@&@�@�@�6@��@l�@E�@-@�@�T@��@%@�@��@�o@h�@c�@:�@ݘ@�f@�@�@�'@��@�!@� @s�@�@��@�@|@�@��@�@��@iD@��@�@�,@8�@��@�@�@��@�@��@u�@S�@Ft@ �@�@�6@�@@��@t�@C�@��@�B@��@��@�@j@B�@4@��@r�@e�@%�@�m@�K@��@/�@�'@��@�+@��@a|@5?@�@�@p�@;@��@��@_@(�@�@�g@��@��@y�@dZ@>�@�@
��@
��@
�h@
�\@
l�@
$�@	��@	�@	�z@	�S@	k�@	-w@	�@�P@��@��@oi@4n@��@qv@9�@$t@�@(@�@�]@��@�@�b@��@h
@c @R�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aз�Aл�Aм6Aл�Aн<Aн<Aм�Aн<Aи�Aж�Aж�AЪ0AК�AЖ�AГ@AЛ=AКAХFA�q�AІ%A�y>A�c A�@A�+6A�%A�$A�#:A�"hA�"4A�"�A�!-A� �A� 'A� 'A� \A��A��A�A��A��A�!A�~A�=A��A�A�
=A��A�zDA���A���AA�U�A�4nA�O�A���A��A���A�kA���A��A��A�F�A�ffA���A��A�%�A�v�A���A�b�A�˒A��A��A�u%A��kA��A���A��dA�ٴA��6A��A�ںA���A�jA���A���A�2aA��YA�.}A���A�.�A��BA�ĜA�)*A�AA{�7AzB[AyY�Av� AtQ�AqYAlAi]�AghsAeԕAcn/Ab!�AaL0A_��AZ��AX��AU��ASC�AN��AN�:AMB[AK(�AG��AE��AB�sA@�QA?�;A>?}A<�A;H�A9�*A8��A81�A7:�A4��A3�A1�A/ѷA.��A.A-g�A+ϫA+u�A+a|A+ѷA)�	A(��A)ߤA){JA(�{A(^5A(=A'�A'6A&�A%�eA$MA"��A!��A!�A |�A�Al�Ae�A�A�hA�AE9AoARTA��A��A�=AM�A�7A��A5?A��AH�AȴA��A��Ak�A<�A1A��A��A7�A�-AxAo A9XA��A��A��AS&A�>A�uAFtA
��A
��A
MjA
�A	�vA	�"A	�Ae,A)�A	A��AoA�=A�QA��A�1A��A�'A�@A�AxlA�A�~A iA��A��A��AaA�UA�AW�A33A�A ��A j�@�m]@�	@��A@���@��	@���@���@�|�@�m�@��@�2a@�!�@��6@�0U@�	l@��@�bN@웦@�t�@�ѷ@�O@�4@�v�@�Y@��@��B@�q@�t�@�:*@�/�@���@��@���@�J#@ެ@��;@�q@ܻ�@�w�@��+@ۂ�@�ߤ@�V�@���@�?�@�@ֶ�@�w�@�R�@՜�@Կ�@�U2@ӈf@�w�@�ԕ@�t�@�'�@а�@�N�@��;@ϕ�@��@�z�@��
@�!-@��U@�'R@�˒@�,�@ʍ�@�@ɿH@ɶF@�a�@�@�֡@��@�.I@���@�y>@ż@�
=@ć�@�v�@�PH@��]@�'�@�<�@��d@�&@���@���@�_�@��[@�<6@���@��@�_�@���@�a�@�L�@�@�ѷ@��@���@�Ov@��@�s�@�8@��@���@�V@�@���@�8@��Y@�D�@�b@�˒@���@���@�c�@�:�@�!�@��@���@��@�O@��{@�P�@�&�@��@���@��+@�t�@�Q�@��@�e�@�خ@���@�~�@�dZ@��j@�9X@���@���@�p�@�Vm@�8�@��@�ی@��1@�6@���@�5�@��s@�2�@��@��T@���@��$@�m]@�7L@��M@��@��@�{�@�D�@��@��:@�hs@�^�@�;d@���@��.@�q@�� @�9�@��@���@��@�'R@��@�x@�%F@�u�@�$�@�J@��Q@�Q�@��v@���@�-�@��o@��@���@�G�@��H@���@�h�@�/�@��t@�;d@��@�ȴ@�J�@��@�Q�@�!�@�C@��@�H@��@�S&@��/@���@��4@�z@�.�@�_@��a@��@�k�@��@���@�W�@��#@���@��@��L@�M@��@��C@���@�y�@�X@�$t@���@���@��.@�(�@���@���@�|�@�N<@�?}@��@��@��@���@��2@�ی@���@��@�V�@�G@���@�!�@�ȴ@��1@�u�@�H@�� @��@���@�s�@�S�@�?}@��@�ߤ@��L@�~(@�I�@��@��m@��@@�J�@��]@��m@���@��}@��m@���@���@�0U@���@�Q�@�@���@���@���@�l�@��*@�Dg@��@�ѷ@��@�i�@�6�@�	@��&@�{J@�S&@�L�@�:�@�@��@�;�@�{@��D@��
@���@��@��@��@��@���@��1@�`�@�7@��P@�=@��P@��'@��\@��@���@���@��@�<6@�	l@��P@��@��X@�B[@�A@x@�@~�m@~��@~L0@~-@}�.@|��@|Ft@{�f@{_p@{o@z��@z�h@z��@zH�@y�#@yL�@x��@x�@x�u@x�@wRT@w8@v�]@v�@uϫ@uc�@u%@tFt@s�A@sqv@s6z@r��@q�o@q<6@p�@p�@o��@oY@n��@n$�@m��@m;@l��@l��@l�.@lj@lU2@k�@@j�"@jJ�@j.�@j#:@j4@i�@iX@i	l@h�@h��@h-�@g1�@fi�@e�#@e�@e#�@d�p@dj@d%�@c��@c]�@cMj@c1�@c$t@c"�@c@b�\@b;�@a�9@a[W@a�@`�[@`��@`�.@``�@`7�@`9X@` �@_�@_��@_��@_,�@^�H@^�1@^d�@^u@]-w@\Ɇ@\U2@\A�@\  @[�a@[��@[8@Z�B@Z�!@Z�X@Zp;@Z$�@Y��@Y\�@X��@W�m@Wl�@WS�@V�@VJ�@U�H@Uq@T��@TPH@TG@S�f@Sn/@R��@Q�@Q��@Q��@Q?}@P�)@P�$@P��@P	�@Oخ@OZ�@N�@NZ�@M�@M?}@M@L�9@Lh�@LH@L'R@K��@K{J@K6z@J��@J�'@J�,@J��@JE�@J�@I�.@I�o@I�X@H��@H'R@G�@G��@GY@F�'@Fu%@F$�@Fu@E��@Ek�@E�@D��@D_@DI�@D*�@Dx@C�@C�w@C�:@C8@Co@B�@B#:@A�@A�7@A@@@�e@@9X@?�@?9�@>�@>�m@>��@>Q@=�@=�7@<�@<��@<S�@;�@;��@;a@;�@:ߤ@:��@:��@:i�@:.�@:@9�-@9�@8�@8U2@7��@71�@6҉@6��@6J�@65?@6+k@6�@6e@6e@5�3@5L�@5�@4�5@4u�@3�Q@3��@3dZ@38@2�m@2�F@2s�@2R�@2GE@2�@1��@1j@1A @1&�@1�@0�K@0�[@0��@0q@01'@/��@/b�@/C�@/.I@.��@.�L@.Ta@.!�@-�>@-�h@-4@-�@,ی@,��@,/�@+خ@+)_@*�@*�y@*�H@*�@*~�@)�)@)�@)��@)c@)�@)��@)c�@)�@(��@(��@(`�@(A�@'��@'>�@'o@'@&��@&�@&+k@%��@%`B@%%F@$��@$<�@#�W@#�;@#��@#$t@"�r@"C�@"O@!��@!�>@!��@!�#@!��@!��@!-w@ �p@ �@ �@ w�@ �@�F@>�@/�@&@�@�@�6@��@l�@E�@-@�@�T@��@%@�@��@�o@h�@c�@:�@ݘ@�f@�@�@�'@��@�!@� @s�@�@��@�@|@�@��@�@��@iD@��@�@�,@8�@��@�@�@��@�@��@u�@S�@Ft@ �@�@�6@�@@��@t�@C�@��@�B@��@��@�@j@B�@4@��@r�@e�@%�@�m@�K@��@/�@�'@��@�+@��@a|@5?@�@�@p�@;@��@��@_@(�@�@�g@��@��@y�@dZ@>�@�@
��@
��@
�h@
�\@
l�@
$�@	��@	�@	�z@	�S@	k�@	-w@	�@�P@��@��@oi@4n@��@qv@9�@$t@�@(@�@�]@��@�@�b@��@h
@c @R�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BPBO�BO�BO�BOvBPBO\BO(BN�BN�BNpBM�BM�BMBM6BM6BL�BMjBL�BK�BK�BK�BJ#BIBH�BH�BIBH�BH�BH�BH�BH�BIBIBIBI7BI7BIBI7BIlBI�BI�BJrBJ�BK)BL�BOB	�B	B	>(B
��B
�8B
�aB
�"B`vB��B��B��BāB�B�$B��BoB�B�B'�B!-BB2BBBSB�B�zB �BB�B�TB��B��B�[BHBezBT�BKB>]BMB
��B
�XB
�3B
c�B
2B
�B	�rB	�B	��B	خB	�7B	��B	��B	��B	{0B	o�B	f�B	XB	PbB	L�B	O�B	JrB	E�B	>BB	2�B	%�B	/ B	:�B	1�B	%FB	�B	_B	HB	6B	xB	�B	MB��B��B��B�zB�ZB��B��B�kB�eB��B�B��B�B	PB	+6B	+6B	%�B	S�B	�{B	��B	�CB	��B	˒B	��B	�1B	ۦB	ںB	ٴB	֡B	�sB	��B	ڠB	��B	ޞB	��B	�pB	߾B	޸B	ߊB	�B	��B	��B	�VB	�jB	�/B	�B	ܒB	��B	�B	�IB	��B	ܒB	�xB	��B	�#B	��B	��B	�WB	یB	�kB	�B	�B	ٴB	��B	��B	ܒB	��B	�]B	�B	��B	�~B	�5B	�~B	��B	�B	�qB	�B	��B	�sB	�9B	�B	�uB	�B	�,B	ԯB	ՁB	ՁB	��B	׍B	�yB	چB	ۦB	ݘB	�~B	�B	ܒB	ۦB	ڠB	��B	՛B	ԯB	�B	өB	�TB	�@B	�:B	�B	�:B	�NB	�}B	�.B	ϫB	��B	οB	��B	�B	�B	�0B	�dB	ˬB	��B	͹B	ΊB	��B	�BB	�BB	�(B	�HB	�B	��B	�.B	�vB	ΊB	ΊB	�B	�B	͹B	�B	�B	҉B	��B	ңB	��B	�B	�4B	��B	�&B	�@B	өB	��B	�&B	өB	өB	��B	��B	�TB	�[B	�B	�B	��B	��B	�B	ևB	��B	��B	׍B	�?B	��B	��B	��B	�QB	�B	�#B	چB	ٴB	ٴB	�B	��B	ٴB	��B	�IB	�!B	�;B	��B	�!B	�-B	�B	��B	�4B	�B	�hB	�B	�TB	�B	��B	��B	�B	��B	�,B	�FB	�B	�B	��B	�8B	�mB	��B	��B	��B	��B	�*B	�DB	�_B	�B	�B	�eB	�B	��B	�QB	�"B	�B	�B	��B	�wB	��B	��B	�B	�}B	�B	�B	�B	�iB	�B	��B	�B	�UB	�B	�B	�AB	�B	�-B	�GB	�B	�B	�TB	�B	�9B	��B	��B	��B	�$B	�rB	�XB	�XB	�rB	��B	��B	�xB	�0B	��B	�PB	��B	��B	�B	��B	��B	��B	��B	�VB	�qB	��B	��B	�]B	��B	�B	�}B	�}B	��B
 iB
 �B
B
oB
�B
oB
oB
�B
B
uB
GB
�B
B
9B
B
B
mB
B
tB
�B
�B
+B
EB
zB
�B
�B
1B
�B
	B
	B
�B
fB
�B
+B
�B
�B
�B
+B
�B
�B
	B
	B
	7B
	RB
	7B
	7B
	7B
	�B
	�B
	�B
	�B

XB

�B
^B
xB
0B
0B
~B
�B
PB
6B
B
6B
�B
�B
B
�B
(B
�B
�B
�B
�B
�B
bB
�B
�B
�B
4B
hB
�B
 B
oB
TB
�B
aB
,B
{B
�B
�B
�B
MB
�B
�B
�B
�B
9B
SB
�B
�B
?B
�B
�B
B
B
�B
�B
�B
�B
�B
_B
�B
�B
�B
�B
�B
B
]B
CB
�B
�B
�B
�B
!B
B
 'B
 BB
 vB
!B
"B
"NB
"NB
"hB
"NB
#�B
$@B
$ZB
$�B
$�B
%,B
&B
&LB
&fB
&�B
&�B
'B
'RB
'�B
)B
)_B
)�B
*KB
*eB
,"B
,WB
,qB
,�B
.B
.IB
./B
./B
.�B
0;B
0oB
0�B
1'B
1[B
1[B
1�B
1�B
1�B
2-B
2B
1�B
1�B
2B
2�B
2�B
2�B
2�B
2�B
33B
33B
3�B
3�B
4TB
4�B
4�B
4�B
4�B
5�B
5�B
6+B
6�B
6�B
7B
6�B
72B
7fB
8B
8B
8B
8�B
9>B
9	B
9>B
8�B
9XB
9XB
9rB
9rB
9�B
9�B
:^B
:DB
9�B
9�B
9XB
9XB
9rB
9�B
9�B
:B
:B
:B
:�B
;dB
;�B
;�B
<jB
<�B
=<B
=<B
=qB
=�B
=qB
=�B
=�B
=qB
=qB
=�B
=�B
>(B
>�B
>�B
>�B
>�B
?.B
?�B
?�B
?�B
@B
@ B
@B
@4B
@�B
@�B
@�B
@�B
AUB
B[B
BuB
B�B
BuB
B�B
B�B
B�B
CGB
C�B
C�B
D�B
EB
E�B
F?B
F?B
FYB
E�B
ESB
E�B
E�B
F%B
F�B
G+B
GzB
G�B
G�B
HKB
HB
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J=B
J=B
JrB
J�B
J�B
K�B
K�B
LB
LJB
LdB
LdB
L~B
L�B
L�B
MB
MjB
M�B
O�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
SB
S&B
S[B
S�B
S�B
TB
TFB
TFB
T,B
T�B
T�B
UB
U2B
U2B
UMB
UMB
UgB
U�B
U�B
U�B
U�B
VB
V9B
VSB
V�B
V�B
V�B
W
B
W�B
W�B
W�B
W�B
XB
X+B
XyB
X�B
X�B
YB
YB
YeB
Y�B
Y�B
ZB
ZB
ZB
ZB
Z7B
ZQB
ZkB
Z�B
Z�B
Z�B
[�B
[�B
\]B
\xB
\�B
\�B
]B
]/B
]B
]B
\�B
]IB
]�B
]�B
]�B
^B
^�B
^�B
^�B
^�B
_;B
_VB
_pB
_�B
_�B
_�B
_�B
`BB
`\B
`\B
`vB
`�B
`�B
`�B
`�B
`�B
`�B
`�B
aB
aB
`�B
a-B
aHB
aB
aHB
a|B
a|B
a�B
a�B
a�B
a�B
bB
cnB
c�B
c�B
c�B
c�B
d&B
d&B
dZB
d@B
d�B
d�B
d�B
eB
e,B
eFB
e�B
fB
fB
f�B
f�B
f�B
f�B
f�B
f�B
gB
gB
gB
gB
g8B
g�B
g�B
g�B
g�B
h$B
iB
iB
iB
i*B
i*B
i*B
iB
iB
iDB
i�B
i�B
jB
jB
i�B
jB
j�B
k6B
kB
k6B
k6B
kQB
k�B
k�B
k�B
k�B
k�B
lB
lB
lqB
l�B
l�B
m)B
m)B
mCB
m)B
mCB
m�B
m�B
ncB
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
pB
poB
qB
qAB
q�B
rB
q�B
q�B
r�B
r�B
s3B
s�B
tB
tB
tB
t9B
tTB
tTB
t�B
t�B
t�B
u%B
uZB
utB
u�B
u�B
vB
v+B
v+B
vzB
wB
wB
v�B
w�B
w�B
w�B
xRB
x�B
x�B
x�B
y>B
y�B
y�B
z*B
z*B
z^B
z�B
z�B
z�B
z�B
{JB
{0B
{B
{�B
{�B
{�B
{�B
|6B
|PB
|6B
|PB
|�B
|�B
|�B
|�B
}"B
}"B
}<B
}�B
}�B
}�B
}�B
}�B
~B
~]B
~]B
~wB
~�B
~�B
~�B
.B
�B
�4B
�iB
��B
��B
��B
��B
��B
��B
��B
��B
� B
�;B
� B
�;11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BPBO�BO�BO�BOvBPBO\BO(BN�BN�BNpBM�BM�BMBM6BM6BL�BMjBL�BK�BK�BK�BJ#BIBH�BH�BIBH�BH�BH�BH�BH�BIBIBIBI7BI7BIBI7BIlBI�BI�BJrBJ�BK)BL�BOB	�B	B	>(B
��B
�8B
�aB
�"B`vB��B��B��BāB�B�$B��BoB�B�B'�B!-BB2BBBSB�B�zB �BB�B�TB��B��B�[BHBezBT�BKB>]BMB
��B
�XB
�3B
c�B
2B
�B	�rB	�B	��B	خB	�7B	��B	��B	��B	{0B	o�B	f�B	XB	PbB	L�B	O�B	JrB	E�B	>BB	2�B	%�B	/ B	:�B	1�B	%FB	�B	_B	HB	6B	xB	�B	MB��B��B��B�zB�ZB��B��B�kB�eB��B�B��B�B	PB	+6B	+6B	%�B	S�B	�{B	��B	�CB	��B	˒B	��B	�1B	ۦB	ںB	ٴB	֡B	�sB	��B	ڠB	��B	ޞB	��B	�pB	߾B	޸B	ߊB	�B	��B	��B	�VB	�jB	�/B	�B	ܒB	��B	�B	�IB	��B	ܒB	�xB	��B	�#B	��B	��B	�WB	یB	�kB	�B	�B	ٴB	��B	��B	ܒB	��B	�]B	�B	��B	�~B	�5B	�~B	��B	�B	�qB	�B	��B	�sB	�9B	�B	�uB	�B	�,B	ԯB	ՁB	ՁB	��B	׍B	�yB	چB	ۦB	ݘB	�~B	�B	ܒB	ۦB	ڠB	��B	՛B	ԯB	�B	өB	�TB	�@B	�:B	�B	�:B	�NB	�}B	�.B	ϫB	��B	οB	��B	�B	�B	�0B	�dB	ˬB	��B	͹B	ΊB	��B	�BB	�BB	�(B	�HB	�B	��B	�.B	�vB	ΊB	ΊB	�B	�B	͹B	�B	�B	҉B	��B	ңB	��B	�B	�4B	��B	�&B	�@B	өB	��B	�&B	өB	өB	��B	��B	�TB	�[B	�B	�B	��B	��B	�B	ևB	��B	��B	׍B	�?B	��B	��B	��B	�QB	�B	�#B	چB	ٴB	ٴB	�B	��B	ٴB	��B	�IB	�!B	�;B	��B	�!B	�-B	�B	��B	�4B	�B	�hB	�B	�TB	�B	��B	��B	�B	��B	�,B	�FB	�B	�B	��B	�8B	�mB	��B	��B	��B	��B	�*B	�DB	�_B	�B	�B	�eB	�B	��B	�QB	�"B	�B	�B	��B	�wB	��B	��B	�B	�}B	�B	�B	�B	�iB	�B	��B	�B	�UB	�B	�B	�AB	�B	�-B	�GB	�B	�B	�TB	�B	�9B	��B	��B	��B	�$B	�rB	�XB	�XB	�rB	��B	��B	�xB	�0B	��B	�PB	��B	��B	�B	��B	��B	��B	��B	�VB	�qB	��B	��B	�]B	��B	�B	�}B	�}B	��B
 iB
 �B
B
oB
�B
oB
oB
�B
B
uB
GB
�B
B
9B
B
B
mB
B
tB
�B
�B
+B
EB
zB
�B
�B
1B
�B
	B
	B
�B
fB
�B
+B
�B
�B
�B
+B
�B
�B
	B
	B
	7B
	RB
	7B
	7B
	7B
	�B
	�B
	�B
	�B

XB

�B
^B
xB
0B
0B
~B
�B
PB
6B
B
6B
�B
�B
B
�B
(B
�B
�B
�B
�B
�B
bB
�B
�B
�B
4B
hB
�B
 B
oB
TB
�B
aB
,B
{B
�B
�B
�B
MB
�B
�B
�B
�B
9B
SB
�B
�B
?B
�B
�B
B
B
�B
�B
�B
�B
�B
_B
�B
�B
�B
�B
�B
B
]B
CB
�B
�B
�B
�B
!B
B
 'B
 BB
 vB
!B
"B
"NB
"NB
"hB
"NB
#�B
$@B
$ZB
$�B
$�B
%,B
&B
&LB
&fB
&�B
&�B
'B
'RB
'�B
)B
)_B
)�B
*KB
*eB
,"B
,WB
,qB
,�B
.B
.IB
./B
./B
.�B
0;B
0oB
0�B
1'B
1[B
1[B
1�B
1�B
1�B
2-B
2B
1�B
1�B
2B
2�B
2�B
2�B
2�B
2�B
33B
33B
3�B
3�B
4TB
4�B
4�B
4�B
4�B
5�B
5�B
6+B
6�B
6�B
7B
6�B
72B
7fB
8B
8B
8B
8�B
9>B
9	B
9>B
8�B
9XB
9XB
9rB
9rB
9�B
9�B
:^B
:DB
9�B
9�B
9XB
9XB
9rB
9�B
9�B
:B
:B
:B
:�B
;dB
;�B
;�B
<jB
<�B
=<B
=<B
=qB
=�B
=qB
=�B
=�B
=qB
=qB
=�B
=�B
>(B
>�B
>�B
>�B
>�B
?.B
?�B
?�B
?�B
@B
@ B
@B
@4B
@�B
@�B
@�B
@�B
AUB
B[B
BuB
B�B
BuB
B�B
B�B
B�B
CGB
C�B
C�B
D�B
EB
E�B
F?B
F?B
FYB
E�B
ESB
E�B
E�B
F%B
F�B
G+B
GzB
G�B
G�B
HKB
HB
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J=B
J=B
JrB
J�B
J�B
K�B
K�B
LB
LJB
LdB
LdB
L~B
L�B
L�B
MB
MjB
M�B
O�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
SB
S&B
S[B
S�B
S�B
TB
TFB
TFB
T,B
T�B
T�B
UB
U2B
U2B
UMB
UMB
UgB
U�B
U�B
U�B
U�B
VB
V9B
VSB
V�B
V�B
V�B
W
B
W�B
W�B
W�B
W�B
XB
X+B
XyB
X�B
X�B
YB
YB
YeB
Y�B
Y�B
ZB
ZB
ZB
ZB
Z7B
ZQB
ZkB
Z�B
Z�B
Z�B
[�B
[�B
\]B
\xB
\�B
\�B
]B
]/B
]B
]B
\�B
]IB
]�B
]�B
]�B
^B
^�B
^�B
^�B
^�B
_;B
_VB
_pB
_�B
_�B
_�B
_�B
`BB
`\B
`\B
`vB
`�B
`�B
`�B
`�B
`�B
`�B
`�B
aB
aB
`�B
a-B
aHB
aB
aHB
a|B
a|B
a�B
a�B
a�B
a�B
bB
cnB
c�B
c�B
c�B
c�B
d&B
d&B
dZB
d@B
d�B
d�B
d�B
eB
e,B
eFB
e�B
fB
fB
f�B
f�B
f�B
f�B
f�B
f�B
gB
gB
gB
gB
g8B
g�B
g�B
g�B
g�B
h$B
iB
iB
iB
i*B
i*B
i*B
iB
iB
iDB
i�B
i�B
jB
jB
i�B
jB
j�B
k6B
kB
k6B
k6B
kQB
k�B
k�B
k�B
k�B
k�B
lB
lB
lqB
l�B
l�B
m)B
m)B
mCB
m)B
mCB
m�B
m�B
ncB
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
pB
poB
qB
qAB
q�B
rB
q�B
q�B
r�B
r�B
s3B
s�B
tB
tB
tB
t9B
tTB
tTB
t�B
t�B
t�B
u%B
uZB
utB
u�B
u�B
vB
v+B
v+B
vzB
wB
wB
v�B
w�B
w�B
w�B
xRB
x�B
x�B
x�B
y>B
y�B
y�B
z*B
z*B
z^B
z�B
z�B
z�B
z�B
{JB
{0B
{B
{�B
{�B
{�B
{�B
|6B
|PB
|6B
|PB
|�B
|�B
|�B
|�B
}"B
}"B
}<B
}�B
}�B
}�B
}�B
}�B
~B
~]B
~]B
~wB
~�B
~�B
~�B
.B
�B
�4B
�iB
��B
��B
��B
��B
��B
��B
��B
��B
� B
�;B
� B
�;11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104937  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174549  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174549  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174549                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024557  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024557  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                