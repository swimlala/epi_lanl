CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-09-26T00:35:26Z creation;2018-09-26T00:35:31Z conversion to V3.1;2019-12-19T07:31:51Z update;     
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
resolution        =���   axis      Z        T  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     T  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  oH   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  s    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  �L   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  �x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     T  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  �L   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ڠ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20180926003526  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0576_284                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @؄5�Ӏ1   @؄6��>�@9��J�M�dC*�0�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
�C  C  C  C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Df��Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�D�|�D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�3D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@\AG�A!G�AAG�AaG�A���A���A���A���A���AУ�A��A��B Q�BQ�BQ�BQ�B Q�B(Q�B0Q�B8Q�B@�RBHQ�BPQ�BXQ�B`Q�BhQ�BpQ�BxQ�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C {C{C{C{C.C
.C{C{C{C.C{C{C{C{C{C{C {C"{C${C&{C({C*{C,{C.{C0{C2{C4{C6{C8{C:{C<{C>{C@{CB{CD{CF{CH{CJ{CL{CN{CP.CR{CT{CV{CX{CZ{C\{C^{C`{Cb{Cd{Cf{Ch{Cj{Cl{Cn{Cp{Cr.Ct.Cv{Cx{Cz{C|{C~{C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=D D �DD�DD�DD�DD�DD�DD�D��D�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�Df��Dg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�DuDu�DvDv�DwDw�DxDx�DyDy�DzDz�D{D{�D|D|�D}D}�D~D~�DD�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D�D�D��D�B�DÂ�D�D��D�B�DĂ�D�D��D�B�Dł�D�D��D�B�DƂ�D�D��D�B�Dǂ�D�D��D�B�DȂ�D�D��D�B�Dɂ�D�D��D�B�Dʂ�D�D��D�B�D˂�D�D��D�B�D̂�D�D��D�B�D͂�D�D��D�B�D΂�D�D��D�B�Dς�D�D��D�B�DЂ�D�D��D�B�Dт�D�D��D�B�D҂�D�D��D�B�Dӂ�D�D��D�B�DԂ�D�D��D�?\D�\D�D��D�B�Dւ�D�D��D�B�Dׂ�D�D��D�B�D؂�D�D��D�B�Dق�D�D��D�B�Dڂ�D�D��D�B�Dۂ�D�D��D�B�D܂�D�D��D�B�D݂�D�D��D�B�Dނ�D�D��D�B�D߂�D�D��D�B�D���D�D��D�B�DႏD�D��D�B�D₏D�D��D�B�DわD�D��D�B�D䂏D�D��D�B�D傏D�D��D�B�D悏D�D��D�B�D炏D�D��D�B�D肏D�D��D�B�D邏D�D��D�B�DꂏD�D��D�B�D낏D�D��D�B�D삏D�D��D�B�D킏D�D��D�B�DD�D��D�B�DD�D��D�B�D���D�D��D�B�D�D�D��D�B�D�D�D��D�B�D�D�D��D�B�D�D�D��D�B�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�Q�A�jA�n�A�l�A�jA�ffA�ffA�ffA�hsA�hsA�jA�n�A�l�A�jA�dZA�ffA�hsA�l�A�l�A�r�A�r�A�p�A�l�A�jA�ffA�S�A���A�ȴAЙ�A���A�ZA��
A�33A�I�AǏ\A��HAhA�E�A�=qA�E�A��A���A���A���A��;A��;A��A���A��A�VA��A�1'A��yA�\)A��PA��A�  A��jA��mA�I�A�jA�l�A�ȴA�z�A���A�ƨA���A�n�A�E�A�ȴA� �A�ffA��!A��A���A�VA�v�A�t�A�;dA�$�A��A�ȴA��A�9XA��^A�%A�l�A�r�A��A��^A�A�A���A�dZA���A���A��HA�t�A�K�A���A��!A��;A�x�A�33A���A���A��+A�ĜA�r�A�
=A�z�A�jA~�HA|Q�A{dZAy�Ax��Aw��Au�At��As�AsoAr��Ar�`Ar��Ar�!Ao�AnVAm�AlZAjI�AiXAf�/AdffAc��Ab�HAbI�Aa|�A`1A^{A\�AZ�+AZQ�AZA�AY�AX��AW�AW\)AV�uAU��ASAP�AO��AM�wALI�AJ�AIVAG��AF��AFv�AF(�AE�-AD�AC��ABE�A@�\A?�A?A<�A:$�A8z�A7�hA7;dA6�A6ffA5��A5�A4�A4ffA3��A3\)A25?A/�wA.�RA-�;A-�7A-?}A,��A,��A,(�A+?}A*JA)&�A'�A'oA%��A%+A%A$�A$��A$JA#�^A#O�A#A"��A"n�A"(�A!G�A ��A �A -A��A�RA�DA{A�;AS�A�+A�#A;dAr�A�^A�/A�
AVA^5A��A&�AbNA�;A��AXA7LA��A�DAbNAƨA�/A�mA��A9XAS�A�RA�A�mA+A
1A	�7A	7LA^5AdZAffA��A�`A1'AbA�wA�hA"�A?}@���@�@�A�@�J@�hs@�I�@�+@�V@�V@�A�@��@�\@�7L@� �@�"�@��-@�r�@�o@���@�7L@�z�@��
@�+@�V@��@�@���@߶F@ݩ�@ە�@ڏ\@ى7@�bN@ׅ@�"�@���@֗�@���@�Q�@��@ѡ�@Ϯ@��@�x�@��@ɲ-@�/@ȓu@�9X@�~�@ģ�@��@�hs@���@�l�@���@��/@��@��\@�J@�x�@��@��@���@��#@�hs@���@�b@��y@�$�@�7L@�bN@��!@���@��-@���@�Ĝ@��u@�Z@�(�@��m@��w@�t�@�o@�M�@���@�X@��@��m@�33@�M�@��/@��@���@��u@�Q�@��m@�l�@�"�@�^5@�@���@��@��9@�Z@��
@�dZ@�
=@���@�M�@�-@�J@�hs@�V@��@��9@���@�r�@�  @���@�t�@�C�@��y@���@�{@���@�O�@��@��@�z�@� �@�ƨ@�S�@���@�@�p�@�V@�j@�1@�ƨ@��P@�K�@���@�$�@���@�p�@�X@�O�@��@��/@��D@�Z@�  @��P@�dZ@�K�@��@��y@�ff@�@�G�@�V@���@���@�z�@� �@�+@��H@��R@�~�@�$�@��T@���@���@�p�@�O�@��9@�Z@��;@�|�@��@���@��@�ȴ@���@���@�n�@�J@��7@��@��@���@��@�Z@�A�@��@�P@|�@\)@~�y@~��@~E�@~@}��@}�h@}p�@}`B@|�@|�j@|�@|1@{�F@{S�@{@z-@yhs@y%@xĜ@xbN@w��@w�@w�P@w|�@w�w@w�;@w��@w�@v�R@v�y@w
=@w
=@w
=@w;d@wK�@v��@u�@u�@vff@vE�@v@u�T@u@u�-@u�h@u?}@t�/@t�j@t�@tz�@tj@t9X@s�F@sS�@so@rM�@q��@q�#@qx�@qG�@q7L@q%@p�9@p�@o�;@ol�@o�@n��@n�R@nV@n$�@n$�@n{@m�T@m�-@l�/@lZ@l�@k�
@k��@kdZ@ko@j�H@j��@j�!@j��@i�@i&�@i�@i�@h��@hr�@g�@g;d@f�R@f��@f��@f�+@fff@fV@f{@e/@d��@dj@d(�@c��@b�@`��@`Q�@_��@_\)@_+@^��@^5?@]�@]�-@]`B@]V@\��@\��@\z�@\9X@[t�@[S�@["�@Z��@Z^5@Y��@Y�7@YG�@Y�@Xr�@X �@W��@W;d@Vȴ@V��@V�+@V5?@U�@U/@T��@T�@T�@T�@T�@T�@T�@T�@T�@T�@T�@T�@T�@T�@Tz�@T�@SdZ@R~�@RM�@Q�@QX@P��@PQ�@P�@PĜ@PĜ@P�u@PA�@P �@O�@O��@O
=@N�@N��@N�+@Nff@NV@NV@NE�@N$�@N@M�T@M��@M�-@M@M�-@M�-@M��@M�@L�@Lj@LI�@L(�@K��@K�F@K�@KS�@J�H@J^5@JM�@J=q@JJ@I��@I��@Ix�@Ix�@I%@H�9@Hr�@G�P@F�@Fff@FE�@F@E�T@E@Ep�@E?}@E/@E�@D��@D�@Dj@DI�@D�@C��@C�@C"�@Co@Co@C@B�H@BM�@BJ@A��@A��@@��@@r�@?�@>��@>��@>v�@=�@=�-@=�-@=p�@<��@<�j@<I�@<9X@<(�@<(�@;�m@;��@;dZ@:�@:M�@:-@9��@9��@:^5@:J@9�@8r�@8�u@81'@7�P@6$�@5@5�@5`B@5/@4�/@4�j@4��@4z�@3�m@3��@3�@3t�@3S�@2��@2-@1�#@1�7@1G�@0��@0��@0b@/��@/
=@.�@.�R@.E�@.@-�T@-@-�-@-��@-p�@-/@-/@,�@,��@,��@,�D@,�D@,Z@,9X@,I�@,I�@+�@+C�@+t�@,(�@,�D@,�j@,��@-V@,��@,�@,�/@,�j@,�D@,j@,9X@+ƨ@+��@+S�@+o@*�!@*n�@*M�@*-@)�@)��@)hs@)hs@(�9@(bN@(1'@'�;@'��@'\)@'K�@'K�@'K�@';d@&�y@&�R@&V@%�@%�-@%`B@%V@$z�@$�@$(�@$1@#��@#��@#�@#S�@#"�@"��@"�\@"=q@!�7@!hs@!7L@!�@ ��@ 1'@K�@;d@
=@ȴ@�+@5?@��@p�@O�@/@�@��@��@��@j@(�@1@ƨ@dZ@S�@33@"�@@�H@�!@��@^5@�#@��@��@X@��@��@�u@bN@1'@ �@ �@A�@A�@ �@��@�;@��@|�@\)@
=@�y@�@�@�@ȴ@�R@��@v�@ff@ff@ff@E�@5?@$�@@�@��@�-@p�@?}@V@�@��@�@�@�D@z�@z�@j@I�@1@ƨ@�@C�@C�@33@o@�H@��@�\@M�@��@%@r�@bN@A�@b@��@+@�+@5?@$�@{@�@��@@@�-@��@�h@p�@�@�j@��@j@j@j@Z@I�@9X@�m@t�@o@o@o@@
�@
�@
�H@
��@
�\@
n�@
=q@
J@	�@	��@	hs@	�@	%@��@�`@��@��@�u@�@bN@Q�@A�@ �@�@�P@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�Q�A�jA�n�A�l�A�jA�ffA�ffA�ffA�hsA�hsA�jA�n�A�l�A�jA�dZA�ffA�hsA�l�A�l�A�r�A�r�A�p�A�l�A�jA�ffA�S�A���A�ȴAЙ�A���A�ZA��
A�33A�I�AǏ\A��HAhA�E�A�=qA�E�A��A���A���A���A��;A��;A��A���A��A�VA��A�1'A��yA�\)A��PA��A�  A��jA��mA�I�A�jA�l�A�ȴA�z�A���A�ƨA���A�n�A�E�A�ȴA� �A�ffA��!A��A���A�VA�v�A�t�A�;dA�$�A��A�ȴA��A�9XA��^A�%A�l�A�r�A��A��^A�A�A���A�dZA���A���A��HA�t�A�K�A���A��!A��;A�x�A�33A���A���A��+A�ĜA�r�A�
=A�z�A�jA~�HA|Q�A{dZAy�Ax��Aw��Au�At��As�AsoAr��Ar�`Ar��Ar�!Ao�AnVAm�AlZAjI�AiXAf�/AdffAc��Ab�HAbI�Aa|�A`1A^{A\�AZ�+AZQ�AZA�AY�AX��AW�AW\)AV�uAU��ASAP�AO��AM�wALI�AJ�AIVAG��AF��AFv�AF(�AE�-AD�AC��ABE�A@�\A?�A?A<�A:$�A8z�A7�hA7;dA6�A6ffA5��A5�A4�A4ffA3��A3\)A25?A/�wA.�RA-�;A-�7A-?}A,��A,��A,(�A+?}A*JA)&�A'�A'oA%��A%+A%A$�A$��A$JA#�^A#O�A#A"��A"n�A"(�A!G�A ��A �A -A��A�RA�DA{A�;AS�A�+A�#A;dAr�A�^A�/A�
AVA^5A��A&�AbNA�;A��AXA7LA��A�DAbNAƨA�/A�mA��A9XAS�A�RA�A�mA+A
1A	�7A	7LA^5AdZAffA��A�`A1'AbA�wA�hA"�A?}@���@�@�A�@�J@�hs@�I�@�+@�V@�V@�A�@��@�\@�7L@� �@�"�@��-@�r�@�o@���@�7L@�z�@��
@�+@�V@��@�@���@߶F@ݩ�@ە�@ڏ\@ى7@�bN@ׅ@�"�@���@֗�@���@�Q�@��@ѡ�@Ϯ@��@�x�@��@ɲ-@�/@ȓu@�9X@�~�@ģ�@��@�hs@���@�l�@���@��/@��@��\@�J@�x�@��@��@���@��#@�hs@���@�b@��y@�$�@�7L@�bN@��!@���@��-@���@�Ĝ@��u@�Z@�(�@��m@��w@�t�@�o@�M�@���@�X@��@��m@�33@�M�@��/@��@���@��u@�Q�@��m@�l�@�"�@�^5@�@���@��@��9@�Z@��
@�dZ@�
=@���@�M�@�-@�J@�hs@�V@��@��9@���@�r�@�  @���@�t�@�C�@��y@���@�{@���@�O�@��@��@�z�@� �@�ƨ@�S�@���@�@�p�@�V@�j@�1@�ƨ@��P@�K�@���@�$�@���@�p�@�X@�O�@��@��/@��D@�Z@�  @��P@�dZ@�K�@��@��y@�ff@�@�G�@�V@���@���@�z�@� �@�+@��H@��R@�~�@�$�@��T@���@���@�p�@�O�@��9@�Z@��;@�|�@��@���@��@�ȴ@���@���@�n�@�J@��7@��@��@���@��@�Z@�A�@��@�P@|�@\)@~�y@~��@~E�@~@}��@}�h@}p�@}`B@|�@|�j@|�@|1@{�F@{S�@{@z-@yhs@y%@xĜ@xbN@w��@w�@w�P@w|�@w�w@w�;@w��@w�@v�R@v�y@w
=@w
=@w
=@w;d@wK�@v��@u�@u�@vff@vE�@v@u�T@u@u�-@u�h@u?}@t�/@t�j@t�@tz�@tj@t9X@s�F@sS�@so@rM�@q��@q�#@qx�@qG�@q7L@q%@p�9@p�@o�;@ol�@o�@n��@n�R@nV@n$�@n$�@n{@m�T@m�-@l�/@lZ@l�@k�
@k��@kdZ@ko@j�H@j��@j�!@j��@i�@i&�@i�@i�@h��@hr�@g�@g;d@f�R@f��@f��@f�+@fff@fV@f{@e/@d��@dj@d(�@c��@b�@`��@`Q�@_��@_\)@_+@^��@^5?@]�@]�-@]`B@]V@\��@\��@\z�@\9X@[t�@[S�@["�@Z��@Z^5@Y��@Y�7@YG�@Y�@Xr�@X �@W��@W;d@Vȴ@V��@V�+@V5?@U�@U/@T��@T�@T�@T�@T�@T�@T�@T�@T�@T�@T�@T�@T�@T�@Tz�@T�@SdZ@R~�@RM�@Q�@QX@P��@PQ�@P�@PĜ@PĜ@P�u@PA�@P �@O�@O��@O
=@N�@N��@N�+@Nff@NV@NV@NE�@N$�@N@M�T@M��@M�-@M@M�-@M�-@M��@M�@L�@Lj@LI�@L(�@K��@K�F@K�@KS�@J�H@J^5@JM�@J=q@JJ@I��@I��@Ix�@Ix�@I%@H�9@Hr�@G�P@F�@Fff@FE�@F@E�T@E@Ep�@E?}@E/@E�@D��@D�@Dj@DI�@D�@C��@C�@C"�@Co@Co@C@B�H@BM�@BJ@A��@A��@@��@@r�@?�@>��@>��@>v�@=�@=�-@=�-@=p�@<��@<�j@<I�@<9X@<(�@<(�@;�m@;��@;dZ@:�@:M�@:-@9��@9��@:^5@:J@9�@8r�@8�u@81'@7�P@6$�@5@5�@5`B@5/@4�/@4�j@4��@4z�@3�m@3��@3�@3t�@3S�@2��@2-@1�#@1�7@1G�@0��@0��@0b@/��@/
=@.�@.�R@.E�@.@-�T@-@-�-@-��@-p�@-/@-/@,�@,��@,��@,�D@,�D@,Z@,9X@,I�@,I�@+�@+C�@+t�@,(�@,�D@,�j@,��@-V@,��@,�@,�/@,�j@,�D@,j@,9X@+ƨ@+��@+S�@+o@*�!@*n�@*M�@*-@)�@)��@)hs@)hs@(�9@(bN@(1'@'�;@'��@'\)@'K�@'K�@'K�@';d@&�y@&�R@&V@%�@%�-@%`B@%V@$z�@$�@$(�@$1@#��@#��@#�@#S�@#"�@"��@"�\@"=q@!�7@!hs@!7L@!�@ ��@ 1'@K�@;d@
=@ȴ@�+@5?@��@p�@O�@/@�@��@��@��@j@(�@1@ƨ@dZ@S�@33@"�@@�H@�!@��@^5@�#@��@��@X@��@��@�u@bN@1'@ �@ �@A�@A�@ �@��@�;@��@|�@\)@
=@�y@�@�@�@ȴ@�R@��@v�@ff@ff@ff@E�@5?@$�@@�@��@�-@p�@?}@V@�@��@�@�@�D@z�@z�@j@I�@1@ƨ@�@C�@C�@33@o@�H@��@�\@M�@��@%@r�@bN@A�@b@��@+@�+@5?@$�@{@�@��@@@�-@��@�h@p�@�@�j@��@j@j@j@Z@I�@9X@�m@t�@o@o@o@@
�@
�@
�H@
��@
�\@
n�@
=q@
J@	�@	��@	hs@	�@	%@��@�`@��@��@�u@�@bN@Q�@A�@ �@�@�P@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B<jBA�BB�BC�BC�BD�BD�BE�BF�BF�BF�BG�BF�BG�BG�BI�BJ�BJ�BN�BN�BM�BO�BR�BbNB�=B��B+BN�BF�Bx�B��B��Bo�Bt�B{�B��B��B��B��B�RB�!B�B��B��B�VB�1B~�By�B{�B{�B�BffBcTBq�BaHB^5BZBVB@�B5?B!�B%�B8RB(�B<jB8RB/B�BB�
B�
B�mB��B�TB�B�wB�3B��B��B�DB|�Bo�Bz�Bk�BcTBR�BB�B<jB7LB+B	7BbB
��B
��B
��B
��B
�mB
��B
ȴB
��B
��B
ÖB
B
�?B
��B
z�B
m�B
v�B
}�B
dZB
?}B
W
B
I�B
<jB
@�B
&�B
1'B
'�B
2-B
6FB
33B
)�B
�B	��B	�B	��B	�mB	��B	ƨB	�?B	��B	�'B	�B	��B	��B	�1B	u�B	{�B	v�B	��B	��B	�bB	� B	� B	{�B	o�B	bNB	@�B	9XB	>wB	-B	"�B	'�B	bB	�B	�B	�B	�B	hB	B�B�ZB�B��B�/B�dB��B�3B�qBǮBŢB�}B�jB�wB�LB�3B�B��B�hB�B��B��B�B�B�B��B��B�oB�B�B}�B�B�B�JB��B��B��B�JB�PB�hB�VB�hB�DB�=B~�B�B�B�Bp�B�1B�+B~�B�Bv�Bn�Bm�BiyBaHBaHB\)BW
BYB]/B]/B[#BVB_;BcTB_;BbNB]/BVBVBI�B>wB8RB9XB;dB9XB;dB;dB?}B49B)�B8RB7LB.B%�B&�B-B-B-B;dB49B0!B#�B%BB&�B�B�B(�B%�B#�B#�B!�B#�B �B$�B�B�B�B�B�B�B�B�B�B�B�B1BDB\B�BJB+BDB{B�B�B�B#�B#�B�B�B\BVBbBDB�B\B%B�B#�B!�B�BoBbB�B�B%�B�B�B&�B&�B-B49B49B1'B2-B49B8RB;dB9XB7LB5?B8RB7LB6FB-B(�B6FBS�B]/B^5B^5B_;B_;B_;B]/B\)BZB]/BbNBaHBZBbNBdZBcTBu�Bw�Bw�Bu�Bt�Bu�Bw�Bv�B{�B}�B|�B�B�%B�B�1B�DB�JB�\B�oB�oB�\B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�3B�XB�^B��BƨBǮBǮBǮBȴB��B�B�B�B�B�B�B�)B�5B�;B�ZB�`B�ZB�`B�TB�`B�B��B��B��B��B��B�B	B	B	B	B		7B	PB	VB	VB	VB	DB	hB	{B	�B	�B	#�B	%�B	(�B	(�B	'�B	&�B	&�B	)�B	-B	33B	7LB	8RB	9XB	>wB	>wB	C�B	E�B	D�B	D�B	F�B	H�B	K�B	L�B	O�B	O�B	P�B	O�B	R�B	T�B	R�B	XB	YB	[#B	\)B	`BB	e`B	hsB	iyB	k�B	p�B	r�B	t�B	v�B	x�B	w�B	y�B	{�B	�B	�B	�%B	�+B	�7B	�7B	�+B	�B	�VB	�oB	�hB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�'B	�'B	�3B	�-B	�3B	�?B	�LB	�LB	�RB	�dB	�jB	�dB	�^B	�^B	�RB	�jB	�}B	��B	B	B	ĜB	ǮB	ȴB	ɺB	ȴB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�B	��B	��B	�B	�B	�
B	��B	��B	��B	��B	�B	�B	�)B	�/B	�BB	�HB	�NB	�NB	�TB	�ZB	�`B	�`B	�`B	�TB	�sB	�sB	�mB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
%B
+B
B
B
%B
%B
B
B
+B
1B
	7B

=B
DB
JB
DB
DB
DB
DB
JB
JB
PB
PB
PB
PB
JB
DB
JB
\B
\B
VB
VB
bB
\B
\B
\B
oB
oB
oB
oB
{B
{B
{B
uB
uB
{B
hB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
#�B
#�B
#�B
'�B
'�B
)�B
/B
)�B
(�B
'�B
-B
+B
)�B
%�B
)�B
,B
-B
-B
-B
.B
0!B
/B
.B
0!B
1'B
1'B
0!B
.B
-B
/B
/B
/B
/B
.B
.B
.B
/B
2-B
33B
2-B
33B
49B
49B
49B
5?B
49B
49B
7LB
6FB
7LB
7LB
8RB
8RB
8RB
8RB
:^B
:^B
8RB
;dB
>wB
B�B
B�B
B�B
B�B
B�B
C�B
C�B
B�B
B�B
B�B
B�B
A�B
@�B
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
A�B
A�B
D�B
C�B
D�B
E�B
G�B
H�B
G�B
G�B
F�B
F�B
F�B
F�B
F�B
H�B
H�B
G�B
I�B
K�B
K�B
K�B
J�B
J�B
K�B
K�B
J�B
K�B
K�B
J�B
L�B
L�B
L�B
K�B
H�B
F�B
K�B
J�B
J�B
J�B
J�B
J�B
M�B
M�B
M�B
N�B
N�B
N�B
M�B
M�B
N�B
N�B
N�B
N�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
Q�B
P�B
R�B
R�B
R�B
R�B
S�B
T�B
S�B
T�B
VB
W
B
W
B
VB
T�B
T�B
W
B
VB
T�B
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
YB
YB
XB
YB
YB
XB
YB
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
[#B
ZB
YB
ZB
ZB
[#B
\)B
\)B
[#B
[#B
[#B
ZB
ZB
XB
YB
[#B
^5B
^5B
]/B
\)B
[#B
\)B
`BB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
bNB
bNB
bNB
aHB
aHB
dZB
dZB
e`B
e`B
e`B
dZB
dZB
cTB
cTB
dZB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
gmB
gmB
gmB
hsB
hsB
gmB
hsB
jB
jB
jB
jB
jB
jB
k�B
jB
k�B
k�B
k�B
jB
jB
jB
l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B<6BAoBB�BC�BC�BD�BD�BE�BF�BF�BF�BG�BF�BG�BG�BI�BJ�BJ�BN�BN�BM�BO�BS&BcB��B՛B-�BQBK�B{�B�zB��Bu�BzxB� B�qB�B��B�8B��B�GB��B�XB�B�B��B�B|PB~B}qB�'Bi�Be,BshBc�B`�B[�BW�BB�B7�B%zB(�B9�B+B<�B8�B/�BB%B�=B�kB�B�^B��BںB� B�B��B�yB�B�BrGB|BmCBd�BU2BD�B>(B8�B,�BB�B
�rB
�2B
��B
��B
��B
�:B
�XB
οB
�PB
�gB
��B
�FB
��B
~B
p�B
xB
~�B
f�B
B�B
XEB
K^B
>B
AoB
)_B
2aB
)yB
2�B
6`B
3hB
*eB
~B	�B	�B	�B	�B	�NB	�1B	�B	�EB	��B	�B	��B	��B	�#B	xRB	}�B	x�B	��B	��B	�B	�oB	��B	|�B	p�B	c�B	C�B	;�B	?�B	/�B	$�B	)�B	�B	�B	�B	KB	#B	 B	gB�tB�LB�=B��B��B�]B��B��B�]B�B�%B�OB�"B��B�B��B��B�B�@B��B��B��B��B��B�wB��B�jB��B��B�gB�B�SB��B�B��B��B��B�6B��B��B��B��B��B��B�4B��B��B��Br|B�B��B�B�UBw�Bo�Bn�BjKBb�BbhB]~BXyBZ7B^B^B\BW
B_�Bc�B_�Bb�B]~BV�BVSBJ�B?�B9�B:�B<�B:�B<PB<PB?�B5tB+�B8�B7�B/iB'8B(XB.B-�B-�B;B4�B0�B$�B�BmB'B �B�B)yB&�B$�B$tB"�B$�B!�B%`B~BdBIB�BWB_B=B 'BIBIB?B	�BdB.BBjB�B~B2B7B_B5B$B$B;BKB}B\BNB�B$BbB�B/B$&B"NBOB�B�B�B�B&�B �B�B'�B'�B-�B4�B4�B1�B2�B4�B8�B;�B9�B8B6B8�B8B7B.�B*�B7�BT{B]IB^jB^OB_VB_�B_pB]dB\xBZ�B]�Bb�Ba�BZ�Bb�Bd�Bd@Bu�Bw�Bw�BvBu%Bv+BxBwfB|B~]B}qB�MB�tB��B��B�xB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�FB�$B�KB�CB�/B�cB�iB�}B��B��B��B��B��B��B��B��B��B�1B�B�BB�B�B�B�KB�_B�kB�CB�jBߊB�ZB�`B�B�zB��B��B��B��B��B�B�B�+B�hB	 B	MB	MB	SB		RB	PB	pB	pB	pB	�B	�B	�B	�B	�B	#�B	%�B	)B	)B	($B	'8B	'RB	*eB	-]B	3MB	7fB	8lB	9�B	>wB	>�B	C�B	E�B	D�B	D�B	F�B	H�B	K�B	MB	O�B	O�B	Q B	O�B	SB	UB	SB	XB	Y1B	[WB	\xB	`�B	ezB	hsB	i�B	k�B	p�B	r�B	t�B	v�B	x�B	w�B	y�B	|6B	�B	�B	�B	�B	�7B	�RB	�_B	��B	�<B	�TB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�8B	�"B	�/B	�IB	�!B	�-B	�AB	�[B	�3B	�|B	�hB	�?B	�fB	�fB	�lB	�dB	�jB	�B	�^B	�xB	��B	��B	��B	��B	B	B	ĶB	ǮB	��B	ɺB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�$B	�B	�B	�B	�@B	�9B	�B	�$B	�FB	� B	�jB	�B	�+B	�KB	�CB	�IB	�\B	�bB	�hB	�B	�nB	�tB	�`B	�zB	�`B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�*B	�B	�.B
B
B
B
9B
9B
?B
?B
9B
MB
EB
1B
	RB

XB
)B
0B
DB
^B
^B
^B
JB
JB
PB
PB
6B
jB
dB
xB
dB
\B
\B
pB
pB
}B
vB
�B
vB
�B
oB
�B
�B
aB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
#�B
#�B
#�B
(
B
(
B
)�B
.�B
*0B
)DB
($B
-B
+6B
*0B
&fB
*B
,B
-)B
-)B
-)B
.B
0!B
/OB
.IB
0;B
1'B
1'B
0;B
.IB
-]B
/B
/5B
/B
/5B
.IB
./B
.IB
/5B
2GB
3MB
2aB
3MB
49B
49B
4TB
5ZB
49B
49B
72B
6FB
7fB
7fB
8RB
88B
8lB
8lB
:^B
:^B
8�B
;B
>]B
BAB
BuB
BuB
BuB
BuB
C�B
C{B
B�B
B�B
B�B
B�B
A�B
@�B
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
A�B
A�B
D�B
C�B
D�B
E�B
G�B
H�B
G�B
G�B
F�B
F�B
F�B
F�B
F�B
H�B
H�B
G�B
I�B
K�B
K�B
K�B
J�B
J�B
K�B
K�B
J�B
K�B
K�B
J�B
L�B
L�B
L�B
K�B
IB
F�B
K�B
J�B
J�B
J�B
J�B
J�B
M�B
M�B
M�B
N�B
N�B
N�B
M�B
M�B
N�B
N�B
N�B
OB
Q�B
Q�B
Q�B
RB
RB
Q�B
SB
RB
QB
SB
SB
R�B
SB
TB
T�B
TB
UB
VB
W
B
W
B
U�B
T�B
UB
W
B
VB
UB
VB
VB
W$B
XB
W�B
W�B
XB
XB
XB
X+B
W�B
YB
YB
X+B
YB
YB
XB
X�B
XB
X+B
X+B
YB
YB
ZB
Z7B
Z7B
[#B
[=B
[	B
[#B
[#B
ZB
Y1B
ZB
ZB
[=B
\B
\B
[=B
[#B
[#B
Z7B
Z7B
XEB
YKB
[=B
^B
^OB
]/B
\CB
[WB
\CB
`\B
bNB
b4B
bhB
bNB
c:B
cTB
cTB
bNB
bNB
bhB
aHB
abB
dZB
dtB
e`B
e`B
eFB
dZB
dtB
c�B
cnB
dtB
gRB
gRB
gRB
gmB
gmB
gmB
gRB
f�B
g�B
g�B
g�B
h�B
hsB
g�B
h�B
jB
jeB
jeB
jB
jB
jeB
k�B
j�B
k�B
k�B
k�B
j�B
j�B
j�B
l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.08(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201809300036162018093000361620180930003616201809300200182018093002001820180930020018201810010023092018100100230920181001002309  JA  ARFMdecpA19c                                                                20180926093524  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180926003526  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180926003529  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180926003529  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180926003530  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180926003530  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180926003530  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180926003530  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180926003530  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180926003531                      G�O�G�O�G�O�                JA  ARUP                                                                        20180926005607                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180926153728  CV  JULD            G�O�G�O�F�!�                JM  ARCAJMQC2.0                                                                 20180929153616  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180929153616  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180929170018  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180930152309  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                