CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:46:08Z creation;2022-06-04T17:46:08Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
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
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604174608  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               
A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @ؽ��41   @ؽ��'�}@.]/��w�d1p��
=1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B���B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���Bә�B���B�  B�  B�  B�  B�  B�B�B�  B���C   C  C�fC  C  C
  C�fC  C  C  C�C33C  C�C�fC�fC   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%fD%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,�fD-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ DҼ�D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�R@��\@\AG�A!G�AAG�AaG�A���A���A���A���A���AУ�A��A��B Q�BQ�B�RBQ�B Q�B(Q�B0Q�B8Q�B@Q�BHQ�BPQ�BXQ�B`Q�BhQ�BpQ�BxQ�B�(�B�(�B�(�B�(�B�(�B���B�B�(�B�\)B�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B�\)B���B�B���B�(�B�(�B�(�B�(�B�(�B�B�B�(�B���C {C{C��C{C{C
{C��C{C{C{C.CG�C{C.C��C��C {C"{C#��C&{C({C*{C,{C.{C0{C2{C4{C6{C8.C:.C<{C>{C@{CB{CD{CF{CH{CJ{CL{CN{CP{CR{CT{CV{CX{CZ{C\{C^{C`{Cb{Cd{Cf{Ch{Cj{Cl.Cn.Cp{Cr{Ct{Cv{Cx{Cz{C|{C~{C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
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
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
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
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
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
=D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%�D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,��D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�DuDu�DvDv�DwDw�DxDx�DyDy�DzDz�D{D{�D|D|�D}D}�D~D~�DD�D��D�E�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D�D�D��D�B�DÂ�D�D��D�B�DĂ�D�D��D�B�Dł�D�D��D�B�DƂ�D�D��D�B�Dǂ�D�D��D�B�DȂ�D�D��D�B�Dɂ�D�D��D�B�Dʂ�D�D��D�B�D˂�D�D��D�B�D̂�D�D��D�B�D͂�D�D��D�B�D΂�D�D��D�B�Dς�D�D��D�B�DЂ�D�D��D�B�Dт�D�D��D�B�D҂�Dҿ\D��D�B�Dӂ�D�D��D�B�DԂ�D�D��D�B�DՂ�D�D��D�B�Dւ�D�D��D�B�Dׂ�D�D��D�B�D؂�D�D��D�B�Dق�D�D��D�B�Dڂ�D�D��D�B�Dۂ�D�D��D�B�D܂�D�D��D�B�D݂�D�D��D�B�Dނ�D�D��D�B�D߂�D�D��D�B�D���D�D��D�B�DႏD�D��D�B�D₏D�D��D�B�DわD�D��D�B�D䂏D�D��D�B�D傏D�D��D�B�D悏D�D��D�B�D��D�D��D�B�D肏D�D��D�B�D邏D�D��D�B�DꂏD�D��D�B�D낏D�D��D�B�D삏D�D��D�B�D킏D�D��D�B�DD�D��D�B�DD�D��D�B�D���D�D��D�B�D�D�D��D�B�D�D�D��D�B�D�D�D��D�B�D�D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D���D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D��)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�&A�1[A�/�A�:�A�5�A��A��|A��KA�V9A�%�A� \A��A�4A��A�_A��A��.A���A��&A�֡A��A�ʌAһdAҧRA�{�A��A�ϫAь�A�]/A�-�A�+6A�$�A�A���AЁA�#A���Aˬ=A�tTA�"�A��A�0�A�+A��A���A�n�A�*�A��/A���A��AƮ�A��AýA��PA��A���A�%�A��VA��A��ZA� �A��mA��A��A��?A��*A�@�A�uA�=�A�v`A�a�A��A��0A�GEA��A�ƨA���A�'�A���A���A���A�tA�_A��A��A��]A�҉A�U2A���A�͟A�k�A�B�A��HA�.�A��A}��AzbNAt�ApqvAl�Ah��Ae�jAcbA]��A\(�AZ��AU��AS��AS?�AR��AP1�AL��AH�AF/AB�)AB?�AA��A@�KA@��A?��A>�A<�oA;��A;'�A:$tA8��A8m]A8jA8f�A7�YA6k�A5-A1m�A08A/�DA.�fA,�=A*E9A)L0A(�AA'�A'zxA&;�A$4�A"�oA"+�A!1�A"hA�zAS�A�?Au�A/�A��AA A�gA�zA�$A��A�	A�	A��A�:A��Au%Aw�A��A	A�A��A|A�A�1Aq�A�+A��A~�AcA\�A�AF�A�3A,�A�AxlA�,AR�Ai�A��A
=AVA}VA��A6zA��A��A��AA�jA��A?�Am]Ac�A
��A
/A
6�A	J�AZ�Aq�A��A	=A	[WA	^�A]�A �A6zA�EA�>A(�A��A��A�AѷA�A�}A�XA�A\�A6zA'RA �HA i�@�H�@�p;@��A@��Q@���@��F@���@��f@�M@�C�@��@���@�hs@���@��@�_@�9X@�_�@� �@��@�%@���@�{�@�kQ@�(�@�e�@�rG@��H@���@�z@�
�@�@��@�{@�+k@�B�@��]@�c @�� @�@�Ta@��@�K�@���@�@�s�@�V�@��@�$@�kQ@�8�@�k@��2@�Ov@��@�ԕ@�4�@�:*@�@�)_@��m@�2�@߶F@߈f@��f@�"h@�S&@���@ܸR@��@ێ"@�@�ff@�G@���@��g@�w�@���@���@׊	@�V@�?@�_p@Թ$@ԍ�@�e�@�@��}@Ӝ@�+�@ҧ�@�A�@�c@�F�@�:�@Ы6@��@�a@�Y@Ζ�@��@͓@�%F@̞@��m@˴�@�~�@�?}@ʩ�@�q@�+k@ɲ-@ɢ�@��@�q@�o @�M�@��z@�|@ĥz@��@�7L@³h@§@§�@«6@ªe@³h@°!@� �@�	l@�a�@���@���@�ƨ@�6z@�@��<@�p;@�>B@��H@�33@�-@� �@�{�@��@�{J@��"@��)@�_@�.I@�@�{J@�>�@��|@���@��D@��-@�rG@�.I@���@�|�@�m�@�\�@�4n@���@���@�x@�Mj@�҉@�D�@�/�@�&�@��>@��@�N<@���@���@���@�A�@��@��@��a@��M@���@��@���@�[�@��@���@�33@���@�u%@�C-@��A@��A@���@��@�ȴ@���@�a|@��A@�l�@��@�?@��@���@�<6@�:�@��@���@���@�GE@�_@���@�{J@�s@�e�@�֡@��K@���@��M@�0�@�ی@���@�a|@���@�'�@��E@���@�A�@�$@�J@��N@���@�O@���@��4@�C�@�@���@�� @��4@�V@��@�K^@�e@��W@��-@�/�@���@���@��@�B[@��@��@��[@��@��@���@��"@�n/@���@�r�@�	@�	�@�u@��@���@�Vm@��@��L@���@�v�@�5?@��;@��@�s�@�K�@���@���@�>B@���@��Q@��]@�*�@�0U@��@�1@���@��@�@���@��K@�x@�g�@�Vm@�@O@�)_@�ی@���@�J@���@���@�x@���@���@�xl@�'R@���@���@�O@���@���@�m�@�6@�O@�_@���@���@�k�@�[W@�U�@���@���@�h
@��]@���@���@��4@�=�@��@��U@�z@�L0@�	�@��K@���@�zx@�Y�@�=@�7L@�4�@�33@��@��`@��E@�Ɇ@���@��@���@�A @��@���@�=q@�&�@��@�~@e�@~v�@~C�@}�@}@@|�@|@{��@{�}@{v`@{1�@{,�@z�2@y�9@y�"@x��@x1'@w��@wZ�@w)_@v�@vff@uVm@t�v@t�@t��@t��@tS�@s�@@r��@ri�@qX@q:�@q\�@qS&@p��@o�
@o8@n҉@n6�@m�^@mL�@l�@lbN@k��@k�@@kqv@j�@j�R@j҉@jR�@i��@if�@i�@h��@h�Y@h�@g�[@gRT@f�"@f�@f@�@e�.@e�'@e+@d��@dɆ@d��@d�@cخ@ce�@b�M@b�1@bOv@a�@arG@`��@`oi@_��@_.I@^��@^�}@^�A@^3�@]�o@]�@]}�@]A @\�5@\�9@\Xy@\*�@[�	@[(@Z�@ZB[@Yԕ@Y�7@YJ�@X�	@Xی@X�@X�@XA�@X�@W��@W�a@WW?@V��@V͟@V�x@VJ�@V#:@V4@V_@V
�@V�@U��@U<6@T��@T �@S�@S��@Sv`@SU�@S33@S$t@R��@R�B@R�A@Q��@Q5�@P�@PV�@O��@On/@N��@Nn�@N0U@M��@MIR@M�@Lی@L�O@Lz�@LD�@K�@K~�@K(@J^5@J	@I��@I0�@H��@H,=@G��@G{J@GW?@G4�@F�y@F��@Fn�@FB[@E��@E�@E��@ErG@EDg@E-w@D��@Db@C�$@C
=@B�@B��@B�r@B-@A5�@A@@�|@@�z@@|�@@ �@?�K@?b�@>�@>�L@>��@>6�@=�#@=X@<ی@<�@<�@<>B@<�@;�6@;X�@:��@:	@9�#@9�@9�~@9j@9`B@9O�@9�@8��@8��@8oi@8H@7ݘ@7�F@7�@7�@@7��@7U�@7F�@6��@6��@6B[@6�@5�@5�~@5V@4�z@4S�@3��@3��@3g�@3�@2�x@2�@1@1:�@0֡@0��@0$@0�@/�Q@/�@/]�@//�@.�s@.:*@-J�@-(�@-�@,�	@,�@,��@,l"@,"h@+�6@+�V@+Mj@+
=@*��@*�+@*s�@*@�@)�@)�=@)4@(�@(�@(V�@((�@'�@'��@'a@'_p@'U�@'+@&��@&ȴ@&�L@&�A@&;�@%@%��@%}�@%A @%�@$��@$��@$�U@$�@$Z@$G@#�@#x@#Z�@#�@"�"@"�B@";�@"�@!��@!��@!��@!��@!p�@!+�@ ��@ c�@ %�@�a@n/@U�@��@��@5?@�@�h@4@�@�D@c�@<�@~@�@��@�$@E9@S@�8@�c@�,@R�@�T@��@s�@O�@2a@@@��@�[@�@�o@[�@��@��@x@8@�@��@��@\�@e@{@��@�T@�@��@?}@ѷ@�@S�@H@ �@�;@��@~�@_p@J#@)_@��@s�@d�@YK@1�@�X@zx@a�@Vm@�@�u@q@<�@�@�[@��@dZ@X�@E9@!-@��@��@��@��@~�@z@v�@J�@�o@�@��@�t@�M@J�@�@�@�@�@��@u�@e�@PH@M@��@�0@s@8@�@@
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�&A�1[A�/�A�:�A�5�A��A��|A��KA�V9A�%�A� \A��A�4A��A�_A��A��.A���A��&A�֡A��A�ʌAһdAҧRA�{�A��A�ϫAь�A�]/A�-�A�+6A�$�A�A���AЁA�#A���Aˬ=A�tTA�"�A��A�0�A�+A��A���A�n�A�*�A��/A���A��AƮ�A��AýA��PA��A���A�%�A��VA��A��ZA� �A��mA��A��A��?A��*A�@�A�uA�=�A�v`A�a�A��A��0A�GEA��A�ƨA���A�'�A���A���A���A�tA�_A��A��A��]A�҉A�U2A���A�͟A�k�A�B�A��HA�.�A��A}��AzbNAt�ApqvAl�Ah��Ae�jAcbA]��A\(�AZ��AU��AS��AS?�AR��AP1�AL��AH�AF/AB�)AB?�AA��A@�KA@��A?��A>�A<�oA;��A;'�A:$tA8��A8m]A8jA8f�A7�YA6k�A5-A1m�A08A/�DA.�fA,�=A*E9A)L0A(�AA'�A'zxA&;�A$4�A"�oA"+�A!1�A"hA�zAS�A�?Au�A/�A��AA A�gA�zA�$A��A�	A�	A��A�:A��Au%Aw�A��A	A�A��A|A�A�1Aq�A�+A��A~�AcA\�A�AF�A�3A,�A�AxlA�,AR�Ai�A��A
=AVA}VA��A6zA��A��A��AA�jA��A?�Am]Ac�A
��A
/A
6�A	J�AZ�Aq�A��A	=A	[WA	^�A]�A �A6zA�EA�>A(�A��A��A�AѷA�A�}A�XA�A\�A6zA'RA �HA i�@�H�@�p;@��A@��Q@���@��F@���@��f@�M@�C�@��@���@�hs@���@��@�_@�9X@�_�@� �@��@�%@���@�{�@�kQ@�(�@�e�@�rG@��H@���@�z@�
�@�@��@�{@�+k@�B�@��]@�c @�� @�@�Ta@��@�K�@���@�@�s�@�V�@��@�$@�kQ@�8�@�k@��2@�Ov@��@�ԕ@�4�@�:*@�@�)_@��m@�2�@߶F@߈f@��f@�"h@�S&@���@ܸR@��@ێ"@�@�ff@�G@���@��g@�w�@���@���@׊	@�V@�?@�_p@Թ$@ԍ�@�e�@�@��}@Ӝ@�+�@ҧ�@�A�@�c@�F�@�:�@Ы6@��@�a@�Y@Ζ�@��@͓@�%F@̞@��m@˴�@�~�@�?}@ʩ�@�q@�+k@ɲ-@ɢ�@��@�q@�o @�M�@��z@�|@ĥz@��@�7L@³h@§@§�@«6@ªe@³h@°!@� �@�	l@�a�@���@���@�ƨ@�6z@�@��<@�p;@�>B@��H@�33@�-@� �@�{�@��@�{J@��"@��)@�_@�.I@�@�{J@�>�@��|@���@��D@��-@�rG@�.I@���@�|�@�m�@�\�@�4n@���@���@�x@�Mj@�҉@�D�@�/�@�&�@��>@��@�N<@���@���@���@�A�@��@��@��a@��M@���@��@���@�[�@��@���@�33@���@�u%@�C-@��A@��A@���@��@�ȴ@���@�a|@��A@�l�@��@�?@��@���@�<6@�:�@��@���@���@�GE@�_@���@�{J@�s@�e�@�֡@��K@���@��M@�0�@�ی@���@�a|@���@�'�@��E@���@�A�@�$@�J@��N@���@�O@���@��4@�C�@�@���@�� @��4@�V@��@�K^@�e@��W@��-@�/�@���@���@��@�B[@��@��@��[@��@��@���@��"@�n/@���@�r�@�	@�	�@�u@��@���@�Vm@��@��L@���@�v�@�5?@��;@��@�s�@�K�@���@���@�>B@���@��Q@��]@�*�@�0U@��@�1@���@��@�@���@��K@�x@�g�@�Vm@�@O@�)_@�ی@���@�J@���@���@�x@���@���@�xl@�'R@���@���@�O@���@���@�m�@�6@�O@�_@���@���@�k�@�[W@�U�@���@���@�h
@��]@���@���@��4@�=�@��@��U@�z@�L0@�	�@��K@���@�zx@�Y�@�=@�7L@�4�@�33@��@��`@��E@�Ɇ@���@��@���@�A @��@���@�=q@�&�@��@�~@e�@~v�@~C�@}�@}@@|�@|@{��@{�}@{v`@{1�@{,�@z�2@y�9@y�"@x��@x1'@w��@wZ�@w)_@v�@vff@uVm@t�v@t�@t��@t��@tS�@s�@@r��@ri�@qX@q:�@q\�@qS&@p��@o�
@o8@n҉@n6�@m�^@mL�@l�@lbN@k��@k�@@kqv@j�@j�R@j҉@jR�@i��@if�@i�@h��@h�Y@h�@g�[@gRT@f�"@f�@f@�@e�.@e�'@e+@d��@dɆ@d��@d�@cخ@ce�@b�M@b�1@bOv@a�@arG@`��@`oi@_��@_.I@^��@^�}@^�A@^3�@]�o@]�@]}�@]A @\�5@\�9@\Xy@\*�@[�	@[(@Z�@ZB[@Yԕ@Y�7@YJ�@X�	@Xی@X�@X�@XA�@X�@W��@W�a@WW?@V��@V͟@V�x@VJ�@V#:@V4@V_@V
�@V�@U��@U<6@T��@T �@S�@S��@Sv`@SU�@S33@S$t@R��@R�B@R�A@Q��@Q5�@P�@PV�@O��@On/@N��@Nn�@N0U@M��@MIR@M�@Lی@L�O@Lz�@LD�@K�@K~�@K(@J^5@J	@I��@I0�@H��@H,=@G��@G{J@GW?@G4�@F�y@F��@Fn�@FB[@E��@E�@E��@ErG@EDg@E-w@D��@Db@C�$@C
=@B�@B��@B�r@B-@A5�@A@@�|@@�z@@|�@@ �@?�K@?b�@>�@>�L@>��@>6�@=�#@=X@<ی@<�@<�@<>B@<�@;�6@;X�@:��@:	@9�#@9�@9�~@9j@9`B@9O�@9�@8��@8��@8oi@8H@7ݘ@7�F@7�@7�@@7��@7U�@7F�@6��@6��@6B[@6�@5�@5�~@5V@4�z@4S�@3��@3��@3g�@3�@2�x@2�@1@1:�@0֡@0��@0$@0�@/�Q@/�@/]�@//�@.�s@.:*@-J�@-(�@-�@,�	@,�@,��@,l"@,"h@+�6@+�V@+Mj@+
=@*��@*�+@*s�@*@�@)�@)�=@)4@(�@(�@(V�@((�@'�@'��@'a@'_p@'U�@'+@&��@&ȴ@&�L@&�A@&;�@%@%��@%}�@%A @%�@$��@$��@$�U@$�@$Z@$G@#�@#x@#Z�@#�@"�"@"�B@";�@"�@!��@!��@!��@!��@!p�@!+�@ ��@ c�@ %�@�a@n/@U�@��@��@5?@�@�h@4@�@�D@c�@<�@~@�@��@�$@E9@S@�8@�c@�,@R�@�T@��@s�@O�@2a@@@��@�[@�@�o@[�@��@��@x@8@�@��@��@\�@e@{@��@�T@�@��@?}@ѷ@�@S�@H@ �@�;@��@~�@_p@J#@)_@��@s�@d�@YK@1�@�X@zx@a�@Vm@�@�u@q@<�@�@�[@��@dZ@X�@E9@!-@��@��@��@��@~�@z@v�@J�@�o@�@��@�t@�M@J�@�@�@�@�@��@u�@e�@PH@M@��@�0@s@8@�@@
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B�_B��B��B�>B�$B��B��B��B��B�
B��B�RB�B��B��B�,B��B�&B��B�B�hB��B��B�pB��B��B��B��B��B��B�@B�:B�|B�RB��B	1�B	i*B	|6B
�B
U�B
}VB
��B
�jB
��B
�	B
�B
�B
��B
�nB
�"B
�BYB(�B:�B]IB~]B��B��B��B��B�ABs�Bt�B��B~�B�B��B��B��B��B�=B��B��B��B��B�BTFB;�B($B
�8B
��B
�jB
�B
�1B
��B
cB
shB
n�B
WsB
>�B
-wB
#�B
uB	�QB	уB	�LB	��B	��B	q�B	dZB	K�B	DgB	<�B	7�B	.IB	-wB	*eB	%B	-�B	3�B	5�B	7�B	BAB	I�B	S�B	R�B	Q�B	R B	W�B	Y�B	VSB	\�B	m�B	o�B	poB	qB	w�B	|�B	�B	��B	��B	�B	�zB	��B	��B	��B	�B	�KB	��B	�dB	�sB	��B	��B	�=B	�B	�'B	�TB	��B	�bB	��B	�:B	��B	��B	�B	��B	�)B	�B	�%B	�dB	�6B	��B	�B	��B	�"B	��B	�B	� B	�B	��B	�B	�UB	�9B	�0B
 B
B
AB
aB	�.B	��B	�
B	�VB	�B	�dB	�B	�IB	�B	��B	�#B	�B	�)B	�QB	��B	��B	��B	��B	ߊB	��B	�B	�LB	��B	�B	�B	�B	�2B	�B	�KB	�B	��B
 B
 �B	�*B	��B	��B
{B
3B
�B
�B
	lB
+B	�B	�aB	��B	��B	�OB	�B	��B	�yB	��B	�>B	�&B	�'B	�OB	��B	�B	� B	�&B	�B	�eB	��B	�"B	�B	�RB	�KB	�>B	��B	��B	��B	�B	��B	�_B	��B	�B	�B	��B	��B	�eB	�B	�5B	�UB	�B	��B	��B	� B	��B	��B	�B	�wB	�B	��B	�cB	��B	�;B	�'B	�B	�|B	��B	��B	��B	�B	�9B	��B	��B	�MB	�B	��B	��B	�B	�B	�B	�B	�B	�9B	�B	�B	�B	��B	�vB	�B	�|B	�'B	��B	�oB	�iB	�B	�B	�B	��B	�TB	�oB	��B	�cB	�CB	��B	�=B	�B	�B	�CB	��B	�B	��B	��B	�B	�5B	��B	��B	�B	�oB	�B	�B	�iB	�OB	��B	�OB	�B	��B	�B	�}B	�cB	�/B	�IB	�OB	�B	�'B	�'B	�B	�B	�5B	�!B	��B	�OB	��B	�B	�'B	�[B	��B	��B	��B	��B	�B	�aB	�UB	��B	�B	�'B	��B	�GB	��B	��B	�B	��B	�+B	�hB	�B	�B	��B	��B	�]B	��B	�CB	�B	�B	�B	�;B	��B	��B	�B	�[B	�B	�B	��B	�B	�B	�B	�B	�B	�nB	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�xB	�JB	�6B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�(B	��B	��B
 iB
;B
�B
B
gB
�B
B
�B
�B
�B
tB
?B
�B
1B
B
fB
	B
	B
	B
	7B
B
�B
B
�B
�B
�B

rB
	�B
	�B
	�B
	�B
	B
	RB
	�B
	7B
	�B
	�B

�B
)B
)B
�B
~B
�B
�B
�B
<B
B
�B
�B
�B
B
�B
B
B
B
PB
�B
�B
�B
<B
pB
"B
\B
�B
�B
&B
FB
�B
�B
B
�B
�B
�B
�B
�B
B
SB
�B
YB
?B
?B
�B

B
mB
�B
mB
�B
MB
MB
�B
B
sB
B
B
B
�B
B
�B
?B
?B
�B
�B
�B
�B
�B
�B
�B
�B
7B
B
QB
�B
�B
/B
B
~B
B
B
jB
B
�B
 �B
!-B
!HB
!bB
"B
"�B
#:B
#�B
#�B
$�B
%zB
%�B
&LB
&LB
&LB
&fB
'B
'B
(
B
($B
(XB
(�B
)*B
)yB
)�B
*B
*�B
*�B
*B
*eB
*�B
+B
+B
+B
+QB
+�B
-B
-]B
-CB
-]B
-�B
.IB
.IB
.B
/OB
/�B
0UB
0�B
1AB
1'B
1[B
1AB
1'B
1vB
1�B
2|B
2�B
2�B
2-B
2�B
3�B
49B
5B
5B
5�B
6B
6B
5�B
6`B
6�B
6�B
6�B
6�B
6�B
8B
72B
7B
9$B
9�B
9�B
8�B
8B
7�B
7fB
7B
7fB
7�B
7�B
8B
7�B
8B
8�B
8�B
9>B
9rB
9�B
:DB
:*B
:�B
;0B
;JB
;dB
;�B
="B
>]B
>�B
>�B
>BB
=�B
=�B
>�B
>�B
?HB
?HB
?cB
?}B
?}B
?�B
@4B
@�B
@�B
@�B
A�B
BAB
B�B
B�B
B�B
CB
B�B
B�B
C{B
C{B
CaB
C�B
C�B
C�B
D�B
E9B
E�B
E�B
E�B
E�B
F%B
F?B
FYB
F�B
FtB
F�B
F�B
F�B
F�B
G+B
G�B
G�B
G�B
G�B
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
H�B
H�B
H�B
H�B
H�B
H�B
H�B
IB
I�B
J	B
J=B
J�B
K^B
K�B
LJB
LdB
LdB
L�B
MPB
MjB
M�B
M�B
M�B
NB
N"B
NpB
N�B
OBB
O\B
O�B
O�B
PHB
P�B
QB
QB
Q4B
Q4B
QhB
Q4B
Q4B
QB
QNB
QhB
QhB
Q�B
Q�B
Q�B
Q�B
RTB
RoB
S&B
S&B
SB
R�B
S[B
TaB
TaB
T�B
UB
T�B
UB
UB
U�B
VB
V9B
VB
VmB
V�B
W
B
W?B
W?B
WYB
W�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
YB
Y1B
Y1B
Y1B
YKB
YeB
Y�B
ZB
Z7B
ZkB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[	B
[qB
[�B
[�B
[�B
[�B
\]B
\�B
\�B
]IB
]~B
]~B
]�B
^B
^�B
^�B
^�B
^�B
^�B
_VB
_!B
_!B
_;B
_pB
_pB
_�B
`�B
a-B
aHB
aHB
aHB
aHB
a|B
a�B
a�B
b4B
bhB
b�B
b�B
cTB
cTB
cnB
c�B
c�B
dB
dtB
d�B
d�B
eFB
e`B
e�B
fB
f2B
fB
fB
ffB
f�B
f�B
f�B
f�B
gB
g�B
g�B
g�B
h
B
h$B
hXB
hXB
hXB
hXB
h�B
h�B
iDB
iDB
iDB
iyB
iyB
i�B
i�B
jB
jKB
j0B
j0B
j0B
jB
jB
kB
kB
kB
k�B
k�B
k�B
l"B
lWB
l�B
l�B
mB
mCB
m�B
m�B
m�B
n/B
nIB
nIB
n}B
n�B
n�B
o5B
o5B
oB
oB
o�B
o�B
p;B
pUB
poB
poB
p�B
p�B
p�B
p�B
p�B
p�B
qvB
q�B
q�B
rB
rB
r-B
r-B
r�B
r�B
r�B
sB
r�B
sB
s3B
s�B
tB
t9B
tTB
tTB
t�B
t�B
t�B
t�B
uB
t�B
uB
u�B
u�B
u�B
u�B
u�B
vzB
v�B
wB
v�B
w2B
v�B
wB
wLB
w�B
w�B
xB
w�B
xB
xB
xRB
x�B
x�B
x�B
y$B
y�B
y�B
y�B
z*B
zxB
z^B
z^B
z^B
z�B
z�B
z�B
{0B
{0B
{dB
{B
{�B
{�B
{�B
|B
|B
|PB
|�B
}B
}"B
}"B
}<11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B�_B��B��B�>B�$B��B��B��B��B�
B��B�RB�B��B��B�,B��B�&B��B�B�hB��B��B�pB��B��B��B��B��B��B�@B�:B�|B�RB��B	1�B	i*B	|6B
�B
U�B
}VB
��B
�jB
��B
�	B
�B
�B
��B
�nB
�"B
�BYB(�B:�B]IB~]B��B��B��B��B�ABs�Bt�B��B~�B�B��B��B��B��B�=B��B��B��B��B�BTFB;�B($B
�8B
��B
�jB
�B
�1B
��B
cB
shB
n�B
WsB
>�B
-wB
#�B
uB	�QB	уB	�LB	��B	��B	q�B	dZB	K�B	DgB	<�B	7�B	.IB	-wB	*eB	%B	-�B	3�B	5�B	7�B	BAB	I�B	S�B	R�B	Q�B	R B	W�B	Y�B	VSB	\�B	m�B	o�B	poB	qB	w�B	|�B	�B	��B	��B	�B	�zB	��B	��B	��B	�B	�KB	��B	�dB	�sB	��B	��B	�=B	�B	�'B	�TB	��B	�bB	��B	�:B	��B	��B	�B	��B	�)B	�B	�%B	�dB	�6B	��B	�B	��B	�"B	��B	�B	� B	�B	��B	�B	�UB	�9B	�0B
 B
B
AB
aB	�.B	��B	�
B	�VB	�B	�dB	�B	�IB	�B	��B	�#B	�B	�)B	�QB	��B	��B	��B	��B	ߊB	��B	�B	�LB	��B	�B	�B	�B	�2B	�B	�KB	�B	��B
 B
 �B	�*B	��B	��B
{B
3B
�B
�B
	lB
+B	�B	�aB	��B	��B	�OB	�B	��B	�yB	��B	�>B	�&B	�'B	�OB	��B	�B	� B	�&B	�B	�eB	��B	�"B	�B	�RB	�KB	�>B	��B	��B	��B	�B	��B	�_B	��B	�B	�B	��B	��B	�eB	�B	�5B	�UB	�B	��B	��B	� B	��B	��B	�B	�wB	�B	��B	�cB	��B	�;B	�'B	�B	�|B	��B	��B	��B	�B	�9B	��B	��B	�MB	�B	��B	��B	�B	�B	�B	�B	�B	�9B	�B	�B	�B	��B	�vB	�B	�|B	�'B	��B	�oB	�iB	�B	�B	�B	��B	�TB	�oB	��B	�cB	�CB	��B	�=B	�B	�B	�CB	��B	�B	��B	��B	�B	�5B	��B	��B	�B	�oB	�B	�B	�iB	�OB	��B	�OB	�B	��B	�B	�}B	�cB	�/B	�IB	�OB	�B	�'B	�'B	�B	�B	�5B	�!B	��B	�OB	��B	�B	�'B	�[B	��B	��B	��B	��B	�B	�aB	�UB	��B	�B	�'B	��B	�GB	��B	��B	�B	��B	�+B	�hB	�B	�B	��B	��B	�]B	��B	�CB	�B	�B	�B	�;B	��B	��B	�B	�[B	�B	�B	��B	�B	�B	�B	�B	�B	�nB	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�xB	�JB	�6B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�(B	��B	��B
 iB
;B
�B
B
gB
�B
B
�B
�B
�B
tB
?B
�B
1B
B
fB
	B
	B
	B
	7B
B
�B
B
�B
�B
�B

rB
	�B
	�B
	�B
	�B
	B
	RB
	�B
	7B
	�B
	�B

�B
)B
)B
�B
~B
�B
�B
�B
<B
B
�B
�B
�B
B
�B
B
B
B
PB
�B
�B
�B
<B
pB
"B
\B
�B
�B
&B
FB
�B
�B
B
�B
�B
�B
�B
�B
B
SB
�B
YB
?B
?B
�B

B
mB
�B
mB
�B
MB
MB
�B
B
sB
B
B
B
�B
B
�B
?B
?B
�B
�B
�B
�B
�B
�B
�B
�B
7B
B
QB
�B
�B
/B
B
~B
B
B
jB
B
�B
 �B
!-B
!HB
!bB
"B
"�B
#:B
#�B
#�B
$�B
%zB
%�B
&LB
&LB
&LB
&fB
'B
'B
(
B
($B
(XB
(�B
)*B
)yB
)�B
*B
*�B
*�B
*B
*eB
*�B
+B
+B
+B
+QB
+�B
-B
-]B
-CB
-]B
-�B
.IB
.IB
.B
/OB
/�B
0UB
0�B
1AB
1'B
1[B
1AB
1'B
1vB
1�B
2|B
2�B
2�B
2-B
2�B
3�B
49B
5B
5B
5�B
6B
6B
5�B
6`B
6�B
6�B
6�B
6�B
6�B
8B
72B
7B
9$B
9�B
9�B
8�B
8B
7�B
7fB
7B
7fB
7�B
7�B
8B
7�B
8B
8�B
8�B
9>B
9rB
9�B
:DB
:*B
:�B
;0B
;JB
;dB
;�B
="B
>]B
>�B
>�B
>BB
=�B
=�B
>�B
>�B
?HB
?HB
?cB
?}B
?}B
?�B
@4B
@�B
@�B
@�B
A�B
BAB
B�B
B�B
B�B
CB
B�B
B�B
C{B
C{B
CaB
C�B
C�B
C�B
D�B
E9B
E�B
E�B
E�B
E�B
F%B
F?B
FYB
F�B
FtB
F�B
F�B
F�B
F�B
G+B
G�B
G�B
G�B
G�B
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
H�B
H�B
H�B
H�B
H�B
H�B
H�B
IB
I�B
J	B
J=B
J�B
K^B
K�B
LJB
LdB
LdB
L�B
MPB
MjB
M�B
M�B
M�B
NB
N"B
NpB
N�B
OBB
O\B
O�B
O�B
PHB
P�B
QB
QB
Q4B
Q4B
QhB
Q4B
Q4B
QB
QNB
QhB
QhB
Q�B
Q�B
Q�B
Q�B
RTB
RoB
S&B
S&B
SB
R�B
S[B
TaB
TaB
T�B
UB
T�B
UB
UB
U�B
VB
V9B
VB
VmB
V�B
W
B
W?B
W?B
WYB
W�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
YB
Y1B
Y1B
Y1B
YKB
YeB
Y�B
ZB
Z7B
ZkB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[	B
[qB
[�B
[�B
[�B
[�B
\]B
\�B
\�B
]IB
]~B
]~B
]�B
^B
^�B
^�B
^�B
^�B
^�B
_VB
_!B
_!B
_;B
_pB
_pB
_�B
`�B
a-B
aHB
aHB
aHB
aHB
a|B
a�B
a�B
b4B
bhB
b�B
b�B
cTB
cTB
cnB
c�B
c�B
dB
dtB
d�B
d�B
eFB
e`B
e�B
fB
f2B
fB
fB
ffB
f�B
f�B
f�B
f�B
gB
g�B
g�B
g�B
h
B
h$B
hXB
hXB
hXB
hXB
h�B
h�B
iDB
iDB
iDB
iyB
iyB
i�B
i�B
jB
jKB
j0B
j0B
j0B
jB
jB
kB
kB
kB
k�B
k�B
k�B
l"B
lWB
l�B
l�B
mB
mCB
m�B
m�B
m�B
n/B
nIB
nIB
n}B
n�B
n�B
o5B
o5B
oB
oB
o�B
o�B
p;B
pUB
poB
poB
p�B
p�B
p�B
p�B
p�B
p�B
qvB
q�B
q�B
rB
rB
r-B
r-B
r�B
r�B
r�B
sB
r�B
sB
s3B
s�B
tB
t9B
tTB
tTB
t�B
t�B
t�B
t�B
uB
t�B
uB
u�B
u�B
u�B
u�B
u�B
vzB
v�B
wB
v�B
w2B
v�B
wB
wLB
w�B
w�B
xB
w�B
xB
xB
xRB
x�B
x�B
x�B
y$B
y�B
y�B
y�B
z*B
zxB
z^B
z^B
z^B
z�B
z�B
z�B
{0B
{0B
{dB
{B
{�B
{�B
{�B
|B
|B
|PB
|�B
}B
}"B
}"B
}<11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104938  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174608  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174608  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174608                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024615  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024615  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                