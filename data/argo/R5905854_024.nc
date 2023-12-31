CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:48:43Z creation;2022-06-04T17:48:44Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604174843  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�����Hp1   @���M��@/M�����cO|�hs1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BXffB`  Bh  Bp  Bx  B�  B�  B�33B�  B���B�  B�  B�  B�33B���B���B�  B�  B�ffB�33B�  B�  B�  B�  B�  B�  B�  B�ffB�33B�33B�  B���B�  B�  B�  B�  B�  C   C  C  C  C33C	�fC  C  C  C  C  C  C  C  C  C  C   C"  C$�C&33C'�fC*  C,  C.  C/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DHfDH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ DҼ�D�  D�@ D�|�D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�s31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�R@��\@\AG�A!G�AAG�AaG�A���A���A���A���A���AУ�A��A��B Q�BQ�BQ�BQ�B Q�B(Q�B0Q�B8Q�B@Q�BH�RBPQ�BX�RB`Q�BhQ�BpQ�BxQ�B�(�B�(�B�\)B�(�B���B�(�B�(�B�(�B�\)B�B���B�(�B�(�B��\B�\)B�(�B�(�B�(�B�(�B�(�B�(�B�(�B؏\B�\)B�\)B�(�B���B�(�B�(�B�(�B�(�B�(�C {C{C{C{CG�C	��C{C{C{C{C{C{C{C{C{C{C {C"{C$.C&G�C'��C*{C,{C.{C/��C2{C4{C6{C8{C:{C<{C>{C@{CB{CD{CF{CH{CJ{CL{CN{CP{CR{CT{CV.CX{CZ{C\{C^{C`{Cb{Cd{Cf{Ch{Cj{Cl{Cn{Cp{Cr{Ct{Cv{Cx{Cz{C|{C~{C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
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
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C��pC��pC�
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
=C�
=C�
=C�
=C�
=C�
=D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DH�DH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�DuDu�DvDv�DwDw�DxDx�DyDy�DzDz�D{D{�D|D|�D}D}�D~D~�DD�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D��\D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D���D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D�D�D��D�B�DÂ�D�D��D�B�DĂ�D�D��D�B�Dł�D�D��D�B�DƂ�D�D��D�B�Dǂ�D�D��D�B�DȂ�D�D��D�B�Dɂ�D�D��D�B�Dʂ�D�D��D�B�D˂�D�D��D�B�D̂�D�D��D�B�D͂�D�D��D�B�D΂�D�D��D�B�Dς�D�D��D�B�DЂ�D�D��D�B�Dт�D�D��D�B�D҂�Dҿ\D��D�B�D�\D�D��D�B�DԂ�D�D��D�B�DՂ�D�D��D�B�Dւ�D�D��D�B�Dׂ�D�D��D�B�D؂�D�D��D�B�Dق�D�D��D�B�Dڂ�D�D��D�B�Dۂ�D�D��D�B�D܂�D�D��D�B�D݂�D�D��D�B�Dނ�D�D��D�B�D߂�D�D��D�B�D���D�D��D�B�DႏD�D��D�B�D₏D�D��D�B�DわD�D��D�B�D䂏D�D��D�B�D傏D�D��D�B�D悏D�D��D�B�D炏D�D��D�B�D肏D�D��D�B�D邏D�D��D�B�DꂏD�D��D�B�D낏D�D��D�B�D삏D�D��D�B�D킏D�D��D�B�DD�D��D�B�DD�D��D�B�D���D�D��D�B�D�D�D��D�B�D�D���D��D�B�D�D�D��D�B�D�D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�E�D���D�D��D�B�D���D�D��D�B�D�u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��)A�9A�L�A�V�A�49A�'�A��A�+A��A��A���A��A��RA߯�Aߘ_A�_pA�CAޠ\A�X�A�"�A��EAݹ�A�xA�XEA�-CA��A�A��	A��Aܗ$A�H�A�&A�\A��PA��Aۏ�AڪeA��A�B'A���A�+AǜCAśqA���A��WA�QNA��CA�_�A��kA��?A��A�l�A���A�\A���A�S�A��1A�'�A���A��'A���A�MA���A��NA�wfA�XEA�sA��+A��$A��A���A�z�A�=�A�V�A��A��A�Z�A�wfA��)A�qA� �A��A|��AtM�Am�Akg�Ai��Ag�Ad�hAc3�Aa�MA^��A[�QAYK^AV/�APzALi�AJb�AF|AC��AB��A>��A;�
A:R�A9
�A7,=A5�fA4�A3RTA2��A2bA19�A/��A.�MA-'RA,($A+XA*v`A*:*A**�A)�9A)K^A(xlA(i�A'QA%��A$��A#�=A#"�A#  A"�A"�9A"�eA";�A!��A!�A!�A!�zA!=A �sA u�A _�A ��A �^A �eA��A��A�A�cAZ�A�A�IA.�AtTAJ#A�`Aa�A�A��A`�AtTA,=A}�A��A�-AOA҉AYA��A��A�eA@OAF�A�#AqvA��Az�AY�A��A:�A	�A�PAMA�`AXyA
خA
�.A
DgA
�A	�TA	,=A��A�'A^�A,�A�|A��A+�A�`A��A~�Ao A[�AA�A%FA�>A�AP�A�A˒A~�A4�A��AL�A6A�AA_�A��A�A \)A ,=@�|@��@�@��y@���@���@�7@�o@�H@���@��=@���@��@��f@���@��"@��*@��@�	@�S@�+k@�@�U�@�.I@��r@@�[�@��@���@��g@�h@�h@�-w@�/�@���@�6@�D@�K^@��@�V@�!�@�y>@�H@�e,@��'@�a�@�2�@��m@��a@�V@�c�@�@���@�>�@���@ݎ�@ܞ�@�4�@ڥz@��@�x@؊r@��@�S@�Xy@��@�6z@ԟ�@�1'@�ƨ@�hs@ҹ�@�  @��@�s�@�@�hs@ν<@���@͎�@�T�@���@�w�@��@��@ʓu@�5?@ɽ�@�F�@Ⱦ@Ǹ�@�S�@ƸR@���@�U�@��E@�M�@��@�ݘ@íC@�8@�-@���@��*@�a@�A @��@��@�H�@���@���@���@�$t@�|�@��N@���@�s@�@��"@��?@�7�@��}@���@��h@�t�@�\�@�C�@���@�!�@���@���@��-@�Z�@��\@�1�@�@���@�@O@���@���@�H@� �@��-@�g�@�*0@��@�;@�ߤ@��b@�0U@�˒@���@�P�@��@�9X@��&@�1�@��5@��@���@�2�@��;@���@�f�@�҉@��]@��q@���@�c�@�@�͟@��@�B[@���@��	@�4�@��	@��`@�c�@�L0@�?�@�C�@��n@�@���@��_@��#@�@��j@���@�a|@���@��X@��@��U@�z@��@��H@���@�w2@���@�H@��@��@��@���@���@�F@��@�z�@�)�@� �@��9@��@���@�g�@�Mj@�'�@��@��z@�C�@��@��a@��'@��@�^�@�(@��9@�~(@�	@���@��'@�j�@�2a@���@���@���@��@�m�@�	@��K@���@��f@��@��!@�S�@�4n@��@���@�ƨ@�n/@��@���@�/�@��T@���@�f�@�J#@�%@��A@�;�@�@�@�f�@�+@��8@�֡@���@�	@��0@�v`@�33@��@�Ɇ@�j@�0U@���@��K@�`B@�F�@�5�@��@��@��@�xl@�4n@�	@��W@���@��{@��@��@�C-@�:�@�5?@��g@���@�O@��@�� @�>B@�@��@��@���@���@�\)@��@��!@�p;@�@�@�x@���@�g�@�>�@�+@���@�Ɇ@���@�� @��@�M@��r@��@��
@���@�s�@���@��)@��9@���@�u�@�\�@�C-@��@���@��*@���@�e,@��@��s@���@�p;@�r�@�@�� @���@�"�@�q�@�
@RT@!-@~R�@}@}�C@}�'@}s�@}L�@}q@|�@|��@|q@{�@{��@{6z@z�H@z�\@z:*@z1�@ze@y��@y��@yj@x�E@x�e@xI�@w�	@v�@vi�@um]@u;@t�?@t|�@tQ�@s�@@r�]@r��@rC�@qj@p�@p�@poi@p"h@o��@odZ@o6z@n�@n�R@n�@m�=@l�p@lh�@l�@k��@k)_@j�,@j�b@i�@iw2@iO�@i�@h��@hS�@g��@g/�@fJ�@e��@e�'@eIR@d��@d*�@cqv@c�@b��@bB[@a��@a@am]@`��@`  @_�w@_��@_��@_�w@_�0@_��@_��@_+@^n�@]��@]��@][W@]@\�f@\��@\PH@[�@[��@[o�@[F�@[)_@[@Z��@Z{�@ZL0@Z+k@Y�T@Y�"@X��@Xq@X�@X  @W�&@W�0@Wb�@V�c@V^5@UT�@T�e@T�@TtT@TPH@T:�@T	�@S�@Sa@S33@So@Rȴ@R;�@R{@Q��@Q�@Q�T@Q��@Q�@Q�@Q@Q��@Q��@Qj@QIR@P�@Pz�@O��@Ob�@O8@N��@N��@Nz@NE�@N �@MT�@L�j@LH@L�@K��@K'�@J�b@J)�@I�-@I��@I�@Izx@I<6@H�)@H��@HtT@He�@H�@G��@Ga@F��@E��@Ea�@E(�@E	l@D��@D�D@C��@Cy�@B��@B�L@Bc @A��@Ac�@AL�@A5�@A#�@A�@A�@@�@@��@@�u@@c�@?��@?~�@?6z@?"�@>�m@>8�@>u@=�@=��@=O�@=#�@=	l@<��@<Ĝ@<�@<K^@;��@;��@;_p@;/�@:��@9��@9@9�@9@@9�@8Ɇ@8z�@8H@8'R@7��@7��@7�:@7�@7g�@7
=@6 �@5w2@5Vm@5 \@4��@4Ĝ@4��@3�V@3@O@2Z�@1ԕ@1�C@1��@1G�@0�@0PH@/��@/�+@/��@/��@/��@/��@/j�@/�@.�X@.kQ@-�@,��@,w�@,M@,-�@,b@+��@+x@+O@+C@*��@*Ta@*M�@*B[@)�>@)�t@)��@)[W@(�@(z�@(<�@($@(1@'خ@'�:@'33@&��@&�X@&ȴ@&��@&h
@&4@%�@%[W@%&�@$�O@#�@#�@#�P@#n/@#4�@#�@"�8@"��@"�h@"��@"c @"Ov@!��@!�@!��@!T�@ ��@ �)@ ��@ �9@ �_@ u�@ �@�@g�@H�@,�@��@�x@h
@@
�@�@��@�@�@�@rG@/@�@��@��@��@��@j@K^@	�@� @�@��@��@~�@\)@�@��@�\@p;@s�@q�@V@�@�T@@��@��@<6@�j@��@q@:�@@��@~�@C@�2@�!@W�@-@	@�)@�M@j@X@N<@�@�@��@u�@oi@g8@Ft@�@��@�k@e�@�@�B@��@v�@1�@��@G�@+�@(�@+�@�@��@��@�@j@@�;@˒@{J@&@S@��@�!@~�@xl@u%@R�@($@_@�o@�-@Vm@=�@&�@�@��@�@�@oi@j@h�@g8@K^@9X@7�@7@�&1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��)A�9A�L�A�V�A�49A�'�A��A�+A��A��A���A��A��RA߯�Aߘ_A�_pA�CAޠ\A�X�A�"�A��EAݹ�A�xA�XEA�-CA��A�A��	A��Aܗ$A�H�A�&A�\A��PA��Aۏ�AڪeA��A�B'A���A�+AǜCAśqA���A��WA�QNA��CA�_�A��kA��?A��A�l�A���A�\A���A�S�A��1A�'�A���A��'A���A�MA���A��NA�wfA�XEA�sA��+A��$A��A���A�z�A�=�A�V�A��A��A�Z�A�wfA��)A�qA� �A��A|��AtM�Am�Akg�Ai��Ag�Ad�hAc3�Aa�MA^��A[�QAYK^AV/�APzALi�AJb�AF|AC��AB��A>��A;�
A:R�A9
�A7,=A5�fA4�A3RTA2��A2bA19�A/��A.�MA-'RA,($A+XA*v`A*:*A**�A)�9A)K^A(xlA(i�A'QA%��A$��A#�=A#"�A#  A"�A"�9A"�eA";�A!��A!�A!�A!�zA!=A �sA u�A _�A ��A �^A �eA��A��A�A�cAZ�A�A�IA.�AtTAJ#A�`Aa�A�A��A`�AtTA,=A}�A��A�-AOA҉AYA��A��A�eA@OAF�A�#AqvA��Az�AY�A��A:�A	�A�PAMA�`AXyA
خA
�.A
DgA
�A	�TA	,=A��A�'A^�A,�A�|A��A+�A�`A��A~�Ao A[�AA�A%FA�>A�AP�A�A˒A~�A4�A��AL�A6A�AA_�A��A�A \)A ,=@�|@��@�@��y@���@���@�7@�o@�H@���@��=@���@��@��f@���@��"@��*@��@�	@�S@�+k@�@�U�@�.I@��r@@�[�@��@���@��g@�h@�h@�-w@�/�@���@�6@�D@�K^@��@�V@�!�@�y>@�H@�e,@��'@�a�@�2�@��m@��a@�V@�c�@�@���@�>�@���@ݎ�@ܞ�@�4�@ڥz@��@�x@؊r@��@�S@�Xy@��@�6z@ԟ�@�1'@�ƨ@�hs@ҹ�@�  @��@�s�@�@�hs@ν<@���@͎�@�T�@���@�w�@��@��@ʓu@�5?@ɽ�@�F�@Ⱦ@Ǹ�@�S�@ƸR@���@�U�@��E@�M�@��@�ݘ@íC@�8@�-@���@��*@�a@�A @��@��@�H�@���@���@���@�$t@�|�@��N@���@�s@�@��"@��?@�7�@��}@���@��h@�t�@�\�@�C�@���@�!�@���@���@��-@�Z�@��\@�1�@�@���@�@O@���@���@�H@� �@��-@�g�@�*0@��@�;@�ߤ@��b@�0U@�˒@���@�P�@��@�9X@��&@�1�@��5@��@���@�2�@��;@���@�f�@�҉@��]@��q@���@�c�@�@�͟@��@�B[@���@��	@�4�@��	@��`@�c�@�L0@�?�@�C�@��n@�@���@��_@��#@�@��j@���@�a|@���@��X@��@��U@�z@��@��H@���@�w2@���@�H@��@��@��@���@���@�F@��@�z�@�)�@� �@��9@��@���@�g�@�Mj@�'�@��@��z@�C�@��@��a@��'@��@�^�@�(@��9@�~(@�	@���@��'@�j�@�2a@���@���@���@��@�m�@�	@��K@���@��f@��@��!@�S�@�4n@��@���@�ƨ@�n/@��@���@�/�@��T@���@�f�@�J#@�%@��A@�;�@�@�@�f�@�+@��8@�֡@���@�	@��0@�v`@�33@��@�Ɇ@�j@�0U@���@��K@�`B@�F�@�5�@��@��@��@�xl@�4n@�	@��W@���@��{@��@��@�C-@�:�@�5?@��g@���@�O@��@�� @�>B@�@��@��@���@���@�\)@��@��!@�p;@�@�@�x@���@�g�@�>�@�+@���@�Ɇ@���@�� @��@�M@��r@��@��
@���@�s�@���@��)@��9@���@�u�@�\�@�C-@��@���@��*@���@�e,@��@��s@���@�p;@�r�@�@�� @���@�"�@�q�@�
@RT@!-@~R�@}@}�C@}�'@}s�@}L�@}q@|�@|��@|q@{�@{��@{6z@z�H@z�\@z:*@z1�@ze@y��@y��@yj@x�E@x�e@xI�@w�	@v�@vi�@um]@u;@t�?@t|�@tQ�@s�@@r�]@r��@rC�@qj@p�@p�@poi@p"h@o��@odZ@o6z@n�@n�R@n�@m�=@l�p@lh�@l�@k��@k)_@j�,@j�b@i�@iw2@iO�@i�@h��@hS�@g��@g/�@fJ�@e��@e�'@eIR@d��@d*�@cqv@c�@b��@bB[@a��@a@am]@`��@`  @_�w@_��@_��@_�w@_�0@_��@_��@_+@^n�@]��@]��@][W@]@\�f@\��@\PH@[�@[��@[o�@[F�@[)_@[@Z��@Z{�@ZL0@Z+k@Y�T@Y�"@X��@Xq@X�@X  @W�&@W�0@Wb�@V�c@V^5@UT�@T�e@T�@TtT@TPH@T:�@T	�@S�@Sa@S33@So@Rȴ@R;�@R{@Q��@Q�@Q�T@Q��@Q�@Q�@Q@Q��@Q��@Qj@QIR@P�@Pz�@O��@Ob�@O8@N��@N��@Nz@NE�@N �@MT�@L�j@LH@L�@K��@K'�@J�b@J)�@I�-@I��@I�@Izx@I<6@H�)@H��@HtT@He�@H�@G��@Ga@F��@E��@Ea�@E(�@E	l@D��@D�D@C��@Cy�@B��@B�L@Bc @A��@Ac�@AL�@A5�@A#�@A�@A�@@�@@��@@�u@@c�@?��@?~�@?6z@?"�@>�m@>8�@>u@=�@=��@=O�@=#�@=	l@<��@<Ĝ@<�@<K^@;��@;��@;_p@;/�@:��@9��@9@9�@9@@9�@8Ɇ@8z�@8H@8'R@7��@7��@7�:@7�@7g�@7
=@6 �@5w2@5Vm@5 \@4��@4Ĝ@4��@3�V@3@O@2Z�@1ԕ@1�C@1��@1G�@0�@0PH@/��@/�+@/��@/��@/��@/��@/j�@/�@.�X@.kQ@-�@,��@,w�@,M@,-�@,b@+��@+x@+O@+C@*��@*Ta@*M�@*B[@)�>@)�t@)��@)[W@(�@(z�@(<�@($@(1@'خ@'�:@'33@&��@&�X@&ȴ@&��@&h
@&4@%�@%[W@%&�@$�O@#�@#�@#�P@#n/@#4�@#�@"�8@"��@"�h@"��@"c @"Ov@!��@!�@!��@!T�@ ��@ �)@ ��@ �9@ �_@ u�@ �@�@g�@H�@,�@��@�x@h
@@
�@�@��@�@�@�@rG@/@�@��@��@��@��@j@K^@	�@� @�@��@��@~�@\)@�@��@�\@p;@s�@q�@V@�@�T@@��@��@<6@�j@��@q@:�@@��@~�@C@�2@�!@W�@-@	@�)@�M@j@X@N<@�@�@��@u�@oi@g8@Ft@�@��@�k@e�@�@�B@��@v�@1�@��@G�@+�@(�@+�@�@��@��@�@j@@�;@˒@{J@&@S@��@�!@~�@xl@u%@R�@($@_@�o@�-@Vm@=�@&�@�@��@�@�@oi@j@h�@g8@K^@9X@7�@7@�&1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	ҽB	�hB	уB	� B	ҽB	ҽB	�uB	�{B	�{B	ԕB	ԯB	՛B	��B	ܬB	ۦB	�B	�oB	�NB	�B	�\B	�\B	��B	׍B	��B	�EB	�EB	׍B	�SB	��B	��B	ѝB	�oB	�@B	өB	уB	� B	��B	�rB	Q�B	Y�B	��B	̈́B	�wB
=VB
K�B
o�B
�hB
�MB
�HBB	B�B"�B!-B!�BH�Bc�Bo�BtBu�BlqB^OBN�BJ�BMjBs�BMBWB�B
�B
�7B
�B
�{B
�B
��B
e�B
L0B
8lB
�B	�B	��B	��B	z*B	[�B	N�B	EmB	<�B	3B	,�B	'�B	�B	+B	}B	�B�zB�0B�IB͹B��B��B�nB�B�B��B��B�WB��B�}B��B�cB��B��B��B��BϫB��BچB�TB�fB��B�0B�"B��B��B��B��B	AB	\B	�B	*0B	2-B	E�B	TB	a�B	s�B	t�B	HB	�B	�B	�B	�|B	�vB	��B	��B	��B	��B	�'B	��B	��B	�{B	��B	��B	�;B	�MB	��B	��B	�wB	�	B	̈́B	��B	ϫB	�.B	�\B	�B	�B	˒B	żB	�SB	�B	ΥB	�\B	ϫB	οB	͹B	�JB	��B	�RB	�=B	ʦB	��B	��B	��B	�B	͟B	��B	�<B	�(B	�B	�\B	�NB	�4B	�hB	�hB	��B	� B	�uB	��B	�B	�?B	רB	�B	�yB	�EB	�+B	��B	��B	��B	�7B	��B	��B	�B	��B	��B	�:B	�B	�;B	ؓB	�B	��B	��B	��B	�9B	��B	�CB	�]B	ܬB	ܬB	�xB	�B	�B	��B	֡B	�mB	ՁB	�B	�B	��B	�vB	�!B	�\B	��B	��B	��B	�B	��B	�B	�IB	�=B	��B	�_B	ּB	خB	��B	�B	�|B	�\B	�FB	�fB	�,B	�B	�B	��B	�4B	�bB	�|B	�pB	��B	ݲB	��B	�B	�B	�jB	�jB	�OB	��B	��B	�B	�HB	�HB	�bB	��B	�NB	�4B	�B	�TB	�B	�nB	�B	�B	�B	��B	�,B	�@B	�@B	�B	�tB	��B	�B	�2B	��B	�zB	��B	�LB	�B	�LB	�B	��B	��B	��B	�B	�)B	�)B	�wB	��B	�B	�B	�B	�IB	�B	��B	��B	�/B	��B	�5B	�iB	�B	�B	�B	�;B	�!B	�oB	�[B	�B	��B	�B	�AB	�B	�B	�B	�vB	�GB	�B	�B	�B	�B	�B	�|B	�GB	��B	�B	��B	�B	�B	��B	�B	��B	��B	��B	�TB	�9B	�B	�B	�B	�B	�B	�B	�%B	�%B	��B	��B	�+B	��B	�2B	�fB	��B	��B	�`B	�B	��B	�RB	��B	��B	�2B	�`B	��B	�2B	�B	��B	�XB	�B	��B	��B	��B	��B	��B	�DB	�B	��B	�B	�JB	��B	�cB	��B	��B	��B
UB
�B
�B
 �B
 �B
 �B
 B
 B
'B
�B
B
-B
GB
B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
EB
zB
�B
�B
�B
B
B
1B
fB
	B
	�B

#B

=B

XB

XB

rB

�B
)B
xB
�B
0B
0B
dB
�B
�B
�B
�B
jB
�B
�B
B
�B
�B
VB
�B
BB
�B
�B
.B
bB
HB
B
�B
 B
�B
�B
@B
&B
�B
{B
�B
{B
�B
aB
�B
�B
�B
B
MB
�B
�B
9B
mB
SB
$B
$B
$B
YB
+B
�B
B
�B
KB
KB
�B
B
7B
�B
B
�B
�B
�B
CB
CB
B
�B
�B
~B
�B
�B
pB
�B
 B
 B
 'B
 �B
!HB
!�B
"B
"NB
"hB
"�B
#nB
$B
$&B
$�B
$�B
%FB
%,B
%B
%B
%FB
%�B
&2B
&B
&2B
'B
'�B
(
B
(XB
(�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+�B
+6B
,B
-)B
-�B
/ B
/�B
0�B
0;B
/�B
.B
-�B
.IB
.IB
/OB
/�B
/�B
/�B
/�B
/�B
0UB
0�B
0�B
1'B
1�B
2B
2�B
2�B
2�B
3hB
3hB
3�B
3�B
3�B
3�B
4nB
4TB
49B
4�B
4�B
5tB
6+B
6FB
6FB
6`B
6`B
6�B
7LB
72B
7fB
8B
8RB
8�B
8�B
8�B
8�B
9	B
9$B
9XB
9rB
9�B
9�B
:xB
:�B
:�B
;B
;B
;�B
;�B
<6B
<PB
<�B
<�B
=VB
=�B
=�B
=�B
=�B
>B
>B
>(B
>wB
>�B
?.B
?B
?�B
?�B
@ B
?�B
@B
AB
B[B
CGB
CaB
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
FB
F%B
F?B
FB
F�B
G+B
G_B
G�B
G�B
G�B
G�B
G�B
HfB
H�B
H�B
H�B
IB
I�B
I�B
J	B
J	B
J#B
J=B
JXB
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
LB
L~B
L�B
L�B
L�B
MB
MjB
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N"B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O\B
O�B
O�B
OvB
O�B
O�B
O�B
PbB
P�B
P�B
P�B
P�B
Q B
Q B
QB
QB
Q4B
Q�B
Q�B
Q�B
RoB
S[B
T,B
TFB
TaB
T�B
T�B
U�B
VB
V�B
V�B
V�B
W?B
WYB
WsB
WsB
W�B
W�B
W�B
W�B
W�B
W�B
XB
X�B
X�B
X�B
X�B
YKB
Y�B
Y�B
Y�B
Y�B
Z7B
Z7B
ZkB
Z�B
Z�B
Z�B
[#B
[#B
[WB
[WB
[=B
[�B
\xB
\�B
\�B
\�B
\�B
\�B
]B
]IB
]IB
]~B
]�B
]�B
]�B
]�B
]�B
^�B
^�B
^�B
_!B
_!B
_;B
_!B
`B
_�B
`�B
`�B
`�B
`�B
`�B
a-B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
bB
bNB
b�B
c:B
cnB
c�B
c�B
c�B
c�B
c�B
dB
c�B
dZB
d�B
dtB
d�B
e,B
e,B
e,B
e`B
e�B
ffB
fLB
fLB
f2B
f2B
f�B
f�B
gmB
gRB
gRB
gmB
g�B
g�B
hsB
h�B
h�B
iB
i�B
i�B
jB
jKB
jeB
jB
jB
j�B
j�B
kB
kB
kB
kkB
k�B
k�B
k�B
lWB
lWB
lWB
lWB
lWB
lqB
l�B
mB
m]B
m]B
mwB
m�B
m�B
n/B
n}B
n}B
n}B
n}B
n�B
n�B
n�B
o B
o5B
oOB
o�B
o�B
o�B
o�B
o�B
o�B
p;B
pUB
pUB
poB
poB
poB
p�B
p�B
q'B
qAB
qvB
q[B
qAB
q[B
q�B
q�B
q�B
rB
q�B
r-B
r|B
r|B
r�B
r�B
r�B
s3B
shB
s�B
s�B
s�B
s�B
tB
tB
tTB
t�B
t�B
u?B
utB
utB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v+B
vzB
v�B
w2B
wLB
wB
wfB
wfB
w�B
x8B
xRB
xlB
xRB
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
zB
zDB
z^B
z�B
z�B
{B
{0B
{B
{JB
{dB
{�B
{�B
{�B
|PB
|�B
|�B
}B
}"B
}<B
}VB
}<B
}"B
}"B
}"B
}VB
}�B
}�B
}�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	ҽB	�hB	уB	� B	ҽB	ҽB	�uB	�{B	�{B	ԕB	ԯB	՛B	��B	ܬB	ۦB	�B	�oB	�NB	�B	�\B	�\B	��B	׍B	��B	�EB	�EB	׍B	�SB	��B	��B	ѝB	�oB	�@B	өB	уB	� B	��B	�rB	Q�B	Y�B	��B	̈́B	�wB
=VB
K�B
o�B
�hB
�MB
�HBB	B�B"�B!-B!�BH�Bc�Bo�BtBu�BlqB^OBN�BJ�BMjBs�BMBWB�B
�B
�7B
�B
�{B
�B
��B
e�B
L0B
8lB
�B	�B	��B	��B	z*B	[�B	N�B	EmB	<�B	3B	,�B	'�B	�B	+B	}B	�B�zB�0B�IB͹B��B��B�nB�B�B��B��B�WB��B�}B��B�cB��B��B��B��BϫB��BچB�TB�fB��B�0B�"B��B��B��B��B	AB	\B	�B	*0B	2-B	E�B	TB	a�B	s�B	t�B	HB	�B	�B	�B	�|B	�vB	��B	��B	��B	��B	�'B	��B	��B	�{B	��B	��B	�;B	�MB	��B	��B	�wB	�	B	̈́B	��B	ϫB	�.B	�\B	�B	�B	˒B	żB	�SB	�B	ΥB	�\B	ϫB	οB	͹B	�JB	��B	�RB	�=B	ʦB	��B	��B	��B	�B	͟B	��B	�<B	�(B	�B	�\B	�NB	�4B	�hB	�hB	��B	� B	�uB	��B	�B	�?B	רB	�B	�yB	�EB	�+B	��B	��B	��B	�7B	��B	��B	�B	��B	��B	�:B	�B	�;B	ؓB	�B	��B	��B	��B	�9B	��B	�CB	�]B	ܬB	ܬB	�xB	�B	�B	��B	֡B	�mB	ՁB	�B	�B	��B	�vB	�!B	�\B	��B	��B	��B	�B	��B	�B	�IB	�=B	��B	�_B	ּB	خB	��B	�B	�|B	�\B	�FB	�fB	�,B	�B	�B	��B	�4B	�bB	�|B	�pB	��B	ݲB	��B	�B	�B	�jB	�jB	�OB	��B	��B	�B	�HB	�HB	�bB	��B	�NB	�4B	�B	�TB	�B	�nB	�B	�B	�B	��B	�,B	�@B	�@B	�B	�tB	��B	�B	�2B	��B	�zB	��B	�LB	�B	�LB	�B	��B	��B	��B	�B	�)B	�)B	�wB	��B	�B	�B	�B	�IB	�B	��B	��B	�/B	��B	�5B	�iB	�B	�B	�B	�;B	�!B	�oB	�[B	�B	��B	�B	�AB	�B	�B	�B	�vB	�GB	�B	�B	�B	�B	�B	�|B	�GB	��B	�B	��B	�B	�B	��B	�B	��B	��B	��B	�TB	�9B	�B	�B	�B	�B	�B	�B	�%B	�%B	��B	��B	�+B	��B	�2B	�fB	��B	��B	�`B	�B	��B	�RB	��B	��B	�2B	�`B	��B	�2B	�B	��B	�XB	�B	��B	��B	��B	��B	��B	�DB	�B	��B	�B	�JB	��B	�cB	��B	��B	��B
UB
�B
�B
 �B
 �B
 �B
 B
 B
'B
�B
B
-B
GB
B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
EB
zB
�B
�B
�B
B
B
1B
fB
	B
	�B

#B

=B

XB

XB

rB

�B
)B
xB
�B
0B
0B
dB
�B
�B
�B
�B
jB
�B
�B
B
�B
�B
VB
�B
BB
�B
�B
.B
bB
HB
B
�B
 B
�B
�B
@B
&B
�B
{B
�B
{B
�B
aB
�B
�B
�B
B
MB
�B
�B
9B
mB
SB
$B
$B
$B
YB
+B
�B
B
�B
KB
KB
�B
B
7B
�B
B
�B
�B
�B
CB
CB
B
�B
�B
~B
�B
�B
pB
�B
 B
 B
 'B
 �B
!HB
!�B
"B
"NB
"hB
"�B
#nB
$B
$&B
$�B
$�B
%FB
%,B
%B
%B
%FB
%�B
&2B
&B
&2B
'B
'�B
(
B
(XB
(�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+�B
+6B
,B
-)B
-�B
/ B
/�B
0�B
0;B
/�B
.B
-�B
.IB
.IB
/OB
/�B
/�B
/�B
/�B
/�B
0UB
0�B
0�B
1'B
1�B
2B
2�B
2�B
2�B
3hB
3hB
3�B
3�B
3�B
3�B
4nB
4TB
49B
4�B
4�B
5tB
6+B
6FB
6FB
6`B
6`B
6�B
7LB
72B
7fB
8B
8RB
8�B
8�B
8�B
8�B
9	B
9$B
9XB
9rB
9�B
9�B
:xB
:�B
:�B
;B
;B
;�B
;�B
<6B
<PB
<�B
<�B
=VB
=�B
=�B
=�B
=�B
>B
>B
>(B
>wB
>�B
?.B
?B
?�B
?�B
@ B
?�B
@B
AB
B[B
CGB
CaB
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
FB
F%B
F?B
FB
F�B
G+B
G_B
G�B
G�B
G�B
G�B
G�B
HfB
H�B
H�B
H�B
IB
I�B
I�B
J	B
J	B
J#B
J=B
JXB
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
LB
L~B
L�B
L�B
L�B
MB
MjB
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N"B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O\B
O�B
O�B
OvB
O�B
O�B
O�B
PbB
P�B
P�B
P�B
P�B
Q B
Q B
QB
QB
Q4B
Q�B
Q�B
Q�B
RoB
S[B
T,B
TFB
TaB
T�B
T�B
U�B
VB
V�B
V�B
V�B
W?B
WYB
WsB
WsB
W�B
W�B
W�B
W�B
W�B
W�B
XB
X�B
X�B
X�B
X�B
YKB
Y�B
Y�B
Y�B
Y�B
Z7B
Z7B
ZkB
Z�B
Z�B
Z�B
[#B
[#B
[WB
[WB
[=B
[�B
\xB
\�B
\�B
\�B
\�B
\�B
]B
]IB
]IB
]~B
]�B
]�B
]�B
]�B
]�B
^�B
^�B
^�B
_!B
_!B
_;B
_!B
`B
_�B
`�B
`�B
`�B
`�B
`�B
a-B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
bB
bNB
b�B
c:B
cnB
c�B
c�B
c�B
c�B
c�B
dB
c�B
dZB
d�B
dtB
d�B
e,B
e,B
e,B
e`B
e�B
ffB
fLB
fLB
f2B
f2B
f�B
f�B
gmB
gRB
gRB
gmB
g�B
g�B
hsB
h�B
h�B
iB
i�B
i�B
jB
jKB
jeB
jB
jB
j�B
j�B
kB
kB
kB
kkB
k�B
k�B
k�B
lWB
lWB
lWB
lWB
lWB
lqB
l�B
mB
m]B
m]B
mwB
m�B
m�B
n/B
n}B
n}B
n}B
n}B
n�B
n�B
n�B
o B
o5B
oOB
o�B
o�B
o�B
o�B
o�B
o�B
p;B
pUB
pUB
poB
poB
poB
p�B
p�B
q'B
qAB
qvB
q[B
qAB
q[B
q�B
q�B
q�B
rB
q�B
r-B
r|B
r|B
r�B
r�B
r�B
s3B
shB
s�B
s�B
s�B
s�B
tB
tB
tTB
t�B
t�B
u?B
utB
utB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v+B
vzB
v�B
w2B
wLB
wB
wfB
wfB
w�B
x8B
xRB
xlB
xRB
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
zB
zDB
z^B
z�B
z�B
{B
{0B
{B
{JB
{dB
{�B
{�B
{�B
|PB
|�B
|�B
}B
}"B
}<B
}VB
}<B
}"B
}"B
}"B
}VB
}�B
}�B
}�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104944  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174843  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174844  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174844                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024851  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024851  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                