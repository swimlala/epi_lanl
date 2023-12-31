CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-02-20T08:01:44Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220220080144  20220220080144  4903174 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               {A   AO  7230                            2B  A   NAVIS_A                         0967                            170425                          863 @ٻ���1   @ٻ��0	@6�I�^5?�cC�;dZ1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         {A   B   F   @�ff@�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^fD^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D���D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@\AG�A!G�AAG�AaG�A���A���A�p�A���A���AУ�A��A��B Q�BQ�BQ�BQ�B Q�B(Q�B0Q�B8Q�B@Q�BHQ�BPQ�BXQ�B`Q�BhQ�BpQ�BxQ�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C {C{C{C{C{C
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
=C�
C�
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
=D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^�D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�DuDu�DvDv�DwDw�DxDx�DyDy�DzDz�D{D{�D|D|�D}D}�D~D~�DD�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D�D�D��D�B�DÂ�D�D��D�B�DĂ�D�D��D�B�Dł�D�D��D�B�DƂ�D�D��D�B�Dǂ�D�D��D�B�DȂ�D�D��D�B�Dɂ�D�D��D�B�Dʂ�D�D��D�B�D˂�D�D��D�B�D̂�D�D��D�B�D͂�D�D��D�B�D΂�D�D��D�B�Dς�D�D��D�B�DЂ�D�D��D�B�Dт�D�D��D�B�D҂�D�D��D�B�Dӂ�D�D��D�B�DԂ�D�D��D�B�DՂ�D�D��D�B�Dւ�D�D��D�B�Dׂ�D�D��D�B�D؂�D�D��D�B�Dق�D�D��\D�B�Dڂ�D�D��D�B�Dۂ�D�D��D�B�D܂�D�D��D�B�D݂�D�D��D�B�Dނ�D�D��D�B�D߂�D�D��D�B�D���D�D��D�B�DႏD�D��D�B�D₏D�D��D�B�DわD�D��D�B�D䂏D�D��D�B�D傏D�D��D�B�D悏D�D��D�B�D炏D�D��D�B�D肏D�D��D�B�D邏D�D��D�B�DꂏD�D��D�B�D낏D�D��D�B�D삏D�D��D�B�D킏D�D��D�B�DD�D��\D�B�DD�D��D�B�D���D�D��D�B�D�D�D��D�B�D�D�D��D�B�D�D�D��D�B�D�D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�E�D�o\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��uA��PA�M�A�l�A�O�A�E�A�A�A�7LA�-A�+A�$�A�"�A��A��A�{A�bA�
=A�%A���A��A���A�A���A���A���A��A�p�A�&�A��;A�jA��/A��9A�\)A�1'A�  A��TA���A�p�A�
=A���A��hA�dZA� �A�
=A���A��#A�ĜA���A�l�A�`BA�XA�Q�A�-A���A��HA���A��9A�t�A��A��7A� �A��
A�1A��A��A��A�v�A��A�l�A�O�A���A��A��PA��A��A���A��#A��PA�I�A��yA�O�A�A�VA�  A�O�A�&�A��A��HA��A���A�7LA�{A�1'A��A��7A���A���A�ȴA��A�Q�A�v�A��hA��A�1'A��A��A���A���A��FA�~�A�ĜA� �A�O�A���A�1AA~�`A}�7A{��Av^5Am"�AlffAkhsAi�Ag��Aex�Ac`BA_�
A^�A]�A[ƨAZ  AVȴAP��AOK�AN{ALE�AK7LAI�AG\)AEG�AB~�A?��A="�A;�
A:��A:�jA:�A7|�A7`BA7\)A6ĜA4^5A3hsA2r�A1��A/�A.v�A-&�A,�/A,v�A,$�A+�A*�\A)�A(��A'O�A&5?A%�A$Q�A"jA!hsA ��A�wA�A�A��A�7A�mA&�A�A�7AXA��A7LA�
AA��A9XA1Ap�A��A
�`A
�A	��A	A	�FA	�7A	t�A	C�A�A(�A�#A��A�PA�jA=qAS�AbNA �A�AbAx�A"�@��R@��9@��P@�C�@��H@���@�bN@�P@��@�t�@��-@�Z@ꗍ@�j@��@��@�"�@�V@��D@݉7@��`@�bN@۶F@��@�  @և+@�E�@��@��@���@��@ӶF@�E�@��@��
@�M�@�`B@�Q�@�\)@ɡ�@�%@��
@��@�V@��@���@�o@��@�j@�b@��m@�|�@��R@�ff@�$�@���@�$�@��R@��R@�p�@�-@���@���@���@���@�z�@���@�~�@�M�@�V@�5?@��@��F@���@���@���@��/@�  @�\)@�33@�
=@��y@���@�-@�p�@���@��@��
@��P@�33@�5?@��@���@�Ĝ@�r�@��
@�\)@�@�n�@��@��^@�%@��`@��`@��`@���@�x�@��@�z�@�bN@�bN@�A�@� �@�  @�l�@�ff@��@���@�Q�@��@�;d@���@��+@�J@�X@�@��-@��@�9X@�I�@�9X@��@��;@�;d@��+@�=q@���@���@���@�x�@�/@��`@��j@��@��@���@�9X@��w@�l�@�o@��H@���@���@���@��@�
=@�+@��H@�v�@��#@���@��7@�`B@�G�@��@��D@��@�b@���@�S�@��@�ȴ@���@�v�@�n�@�5?@�J@��@���@��h@��@�?}@��j@��@�Z@�A�@��;@�t�@�;d@�+@�
=@���@�n�@�$�@��#@��^@���@�x�@�G�@�%@��/@���@�Ĝ@���@��@��@��D@�z�@�I�@�I�@���@��w@��@�dZ@�K�@��@�ȴ@�~�@�ff@�M�@�$�@�@��@��T@��#@��#@��T@��#@���@�@���@��7@�hs@�G�@��@���@��/@��j@��D@�A�@���@��F@��@���@�|�@�\)@���@��!@�~�@�E�@�@��#@��h@�x�@�p�@�hs@�O�@��@���@��9@���@��@�j@��@�ƨ@��P@�\)@�;d@�@��@���@���@�E�@�{@��T@���@���@��@�G�@�O�@�&�@���@��j@�r�@�9X@�  @\)@
=@~��@~�y@~�@~��@~$�@}�T@}��@}p�@}V@|��@|z�@|(�@{�@{"�@z��@zn�@y�@yX@y�@y%@x��@xĜ@xbN@x1'@w�@v�R@v��@v��@v�+@vv�@vff@vE�@u@t��@t�D@t9X@t1@t1@s�m@st�@sC�@sC�@so@r�H@r��@rn�@r-@q�@q��@q��@q7L@p��@p  @o��@o;d@o
=@n��@n�@n��@n$�@m��@m/@l�@l��@lj@l(�@k��@kS�@j��@i��@i�7@i�7@iX@i7L@i&�@i%@h�9@hr�@hb@gK�@g+@g
=@f��@f��@f�y@f�@f�R@fff@e�T@e?}@d��@d��@c��@c��@cC�@b��@bM�@a��@a�7@a7L@`��@`�9@`bN@`b@_��@_+@^��@^$�@]��@]`B@]V@\�@[S�@Z��@Z=q@Y��@Y��@YG�@X�`@X�u@X  @W�@W�w@W\)@W
=@Vȴ@V�R@Vv�@V$�@V{@U�h@U/@T�j@TZ@T�@S�m@S�F@S33@R��@R^5@Q��@Q�^@Q��@Qhs@Q&�@P��@PbN@P1'@O�w@O��@Ol�@N�R@Nff@N$�@M�-@MO�@L��@L��@L9X@K��@K�F@K�@KS�@J�H@J~�@J^5@J�@Ix�@IG�@I�@H�@G�;@F��@Fv�@FE�@FE�@F5?@F{@E�T@E�-@E`B@EO�@D�@D(�@C��@C�m@C�
@C�
@Cƨ@C��@C�@CdZ@CC�@Co@B��@B^5@BJ@A��@Ax�@AX@A7L@A�@A%@@��@@��@@�`@@�u@@bN@?�w@?�P@?�P@?;d@>��@>�y@>�y@>�y@>�@>�@>ȴ@=�@=�@=O�@=/@=�@=V@<�/@<��@;��@;�F@;dZ@;"�@:��@:�\@:J@9��@9X@8�u@8r�@81'@8b@8b@8  @7�w@7l�@7�@6ȴ@6�R@6�R@6��@6�+@6{@5�@5`B@5/@4��@4�@4�j@4�D@49X@3��@3t�@3C�@3"�@2�!@2~�@2=q@1�^@1��@1��@1�7@1hs@0Ĝ@0r�@0bN@0A�@0 �@/�;@/��@/+@.��@.ȴ@.ff@-��@-`B@-�@,�@,�@,j@,(�@,1@+�m@+ƨ@+��@+S�@+@*��@*��@*��@*�\@*^5@*M�@*=q@*J@)�#@)�7@(��@(�@'�;@'l�@'+@&�@&�R@&�+@&$�@&{@&@%�T@%@%�-@%�h@%/@%�@%V@$j@#��@#dZ@#33@"�@"��@"��@"�!@"�!@"~�@"n�@"-@"�@!�#@!��@!hs@!G�@!�@!�@!%@ �9@ �@ 1'@��@�@��@|�@\)@;d@��@v�@E�@$�@�T@�h@?}@/@�@��@�D@Z@I�@9X@1@�F@��@�@S�@S�@33@"�@�H@~�@~�@M�@�#@��@hs@G�@7L@&�@%@��@��@�`@Ĝ@bN@��@�@�@ȴ@�R@v�@E�@�@�h@�@�@O�@�@V@�@��@�@j@I�@9X@(�@��@�F@�F@��@�@dZ@33@o@�@�H@��@~�@M�@-@��@�#@��@��@hs@7L@%@Ĝ@��@Q�@�@�w@\)@�@��@�@�R@��@v�@V@$�@��@@�-@�-@�-@�-@��@�h@p�@�@V@V@�/@��@�D@z�@j@Z@(�@��@��@dZ@33@@
�@
��@
�\@
n�@
M�@
=q@
�@	�@	�^@	�7@	hs@	7L@��@r�@Q�@A�@b@ �@b@��@�w@��@|�@\)@�@��@v�@V@V11111111111111111111111111111111111111111111111111111111444444111111111111111111441111111144444441111111114444441111111111111111111111114444444411441444111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114444444441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114444444441111111111111111111114444444441111111111111111111111111111111111114411444414444444441111111111111111111111144   A��uA��PA�M�A�l�A�O�A�E�A�A�A�7LA�-A�+A�$�A�"�A��A��A�{A�bA�
=A�%A���A��A���A�A���A���A���A��A�p�A�&�A��;A�jA��/A��9A�\)A�1'A�  A��TA���A�p�A�
=A���A��hA�dZA� �A�
=A���A��#A�ĜA���A�l�A�`BA�XA�Q�A�-A���A��HA���A��9A�t�A��A��7A� �A��
A�1A��A��A��A�v�A��A�l�A�O�A���A��A��PA��A��A���A��#A��PA�I�A��yA�O�A�A�VA�  A�O�A�&�A��A��HA��A���A�7LA�{A�1'A��A��7A���A���A�ȴA��A�Q�A�v�A��hA��A�1'A��A��A���A���A��FA�~�A�ĜA� �A�O�A���A�1AA~�`A}�7A{��Av^5Am"�AlffAkhsAi�Ag��Aex�Ac`BA_�
A^�A]�A[ƨAZ  AVȴAP��AOK�AN{ALE�AK7LAI�AG\)AEG�AB~�A?��A="�A;�
A:��A:�jA:�A7|�A7`BA7\)A6ĜA4^5A3hsA2r�A1��A/�A.v�A-&�A,�/A,v�A,$�A+�A*�\A)�A(��A'O�A&5?A%�A$Q�A"jA!hsA ��A�wA�A�A��A�7A�mA&�A�A�7AXA��A7LA�
AA��A9XA1Ap�A��A
�`A
�A	��A	A	�FA	�7A	t�A	C�A�A(�A�#A��A�PA�jA=qAS�AbNA �A�AbAx�A"�@��R@��9@��P@�C�@��H@���@�bN@�P@��@�t�@��-@�Z@ꗍ@�j@��@��@�"�@�V@��D@݉7@��`@�bN@۶F@��@�  @և+@�E�@��@��@���@��@ӶF@�E�@��@��
@�M�@�`B@�Q�@�\)@ɡ�@�%@��
@��@�V@��@���@�o@��@�j@�b@��m@�|�@��R@�ff@�$�@���@�$�@��R@��R@�p�@�-@���@���@���@���@�z�@���@�~�@�M�@�V@�5?@��@��F@���@���@���@��/@�  @�\)@�33@�
=@��y@���@�-@�p�@���@��@��
@��P@�33@�5?@��@���@�Ĝ@�r�@��
@�\)@�@�n�@��@��^@�%@��`@��`@��`@���@�x�@��@�z�@�bN@�bN@�A�@� �@�  @�l�@�ff@��@���@�Q�@��@�;d@���@��+@�J@�X@�@��-@��@�9X@�I�@�9X@��@��;@�;d@��+@�=q@���@���@���@�x�@�/@��`@��j@��@��@���@�9X@��w@�l�@�o@��H@���@���@���@��@�
=@�+@��H@�v�@��#@���@��7@�`B@�G�@��@��D@��@�b@���@�S�@��@�ȴ@���@�v�@�n�@�5?@�J@��@���@��h@��@�?}@��j@��@�Z@�A�@��;@�t�@�;d@�+@�
=@���@�n�@�$�@��#@��^@���@�x�@�G�@�%@��/@���@�Ĝ@���@��@��@��D@�z�@�I�@�I�@���@��w@��@�dZ@�K�@��@�ȴ@�~�@�ff@�M�@�$�@�@��@��T@��#@��#@��T@��#@���@�@���@��7@�hs@�G�@��@���@��/@��j@��D@�A�@���@��F@��@���@�|�@�\)@���@��!@�~�@�E�@�@��#@��h@�x�@�p�@�hs@�O�@��@���@��9@���@��@�j@��@�ƨ@��P@�\)@�;d@�@��@���@���@�E�@�{@��T@���@���@��@�G�@�O�@�&�@���@��j@�r�@�9X@�  @\)@
=@~��@~�y@~�@~��@~$�@}�T@}��@}p�@}V@|��@|z�@|(�@{�@{"�@z��@zn�@y�@yX@y�@y%@x��@xĜ@xbN@x1'@w�@v�R@v��@v��@v�+@vv�@vff@vE�@u@t��@t�D@t9X@t1@t1@s�m@st�@sC�@sC�@so@r�H@r��@rn�@r-@q�@q��@q��@q7L@p��@p  @o��@o;d@o
=@n��@n�@n��@n$�@m��@m/@l�@l��@lj@l(�@k��@kS�@j��@i��@i�7@i�7@iX@i7L@i&�@i%@h�9@hr�@hb@gK�@g+@g
=@f��@f��@f�y@f�@f�R@fff@e�T@e?}@d��@d��@c��@c��@cC�@b��@bM�@a��@a�7@a7L@`��@`�9@`bN@`b@_��@_+@^��@^$�@]��@]`B@]V@\�@[S�@Z��@Z=q@Y��@Y��@YG�@X�`@X�u@X  @W�@W�w@W\)@W
=@Vȴ@V�R@Vv�@V$�@V{@U�h@U/@T�j@TZ@T�@S�m@S�F@S33@R��@R^5@Q��@Q�^@Q��@Qhs@Q&�@P��@PbN@P1'@O�w@O��@Ol�@N�R@Nff@N$�@M�-@MO�@L��@L��@L9X@K��@K�F@K�@KS�@J�H@J~�@J^5@J�@Ix�@IG�@I�@H�@G�;@F��@Fv�@FE�@FE�@F5?@F{@E�T@E�-@E`B@EO�@D�@D(�@C��@C�m@C�
@C�
@Cƨ@C��@C�@CdZ@CC�@Co@B��@B^5@BJ@A��@Ax�@AX@A7L@A�@A%@@��@@��@@�`@@�u@@bN@?�w@?�P@?�P@?;d@>��@>�y@>�y@>�y@>�@>�@>ȴ@=�@=�@=O�@=/@=�@=V@<�/@<��@;��@;�F@;dZ@;"�@:��@:�\@:J@9��@9X@8�u@8r�@81'@8b@8b@8  @7�w@7l�@7�@6ȴ@6�R@6�R@6��@6�+@6{@5�@5`B@5/@4��@4�@4�j@4�D@49X@3��@3t�@3C�@3"�@2�!@2~�@2=q@1�^@1��@1��@1�7@1hs@0Ĝ@0r�@0bN@0A�@0 �@/�;@/��@/+@.��@.ȴ@.ff@-��@-`B@-�@,�@,�@,j@,(�@,1@+�m@+ƨ@+��@+S�@+@*��@*��@*��@*�\@*^5@*M�@*=q@*J@)�#@)�7@(��@(�@'�;@'l�@'+@&�@&�R@&�+@&$�@&{@&@%�T@%@%�-@%�h@%/@%�@%V@$j@#��@#dZ@#33@"�@"��@"��@"�!@"�!@"~�@"n�@"-@"�@!�#@!��@!hs@!G�@!�@!�@!%@ �9@ �@ 1'@��@�@��@|�@\)@;d@��@v�@E�@$�@�T@�h@?}@/@�@��@�D@Z@I�@9X@1@�F@��@�@S�@S�@33@"�@�H@~�@~�@M�@�#@��@hs@G�@7L@&�@%@��@��@�`@Ĝ@bN@��@�@�@ȴ@�R@v�@E�@�@�h@�@�@O�@�@V@�@��@�@j@I�@9X@(�@��@�F@�F@��@�@dZ@33@o@�@�H@��@~�@M�@-@��@�#@��@��@hs@7L@%@Ĝ@��@Q�@�@�w@\)@�@��@�@�R@��@v�@V@$�@��@@�-@�-@�-@�-@��@�h@p�@�@V@V@�/@��@�D@z�@j@Z@(�@��@��@dZ@33@@
�@
��@
�\@
n�@
M�@
=q@
�@	�@	�^@	�7@	hs@	7L@��@r�@Q�@A�@b@ �@b@��@�w@��@|�@\)@�@��@v�@V@V11111111111111111111111111111111111111111111111111111111444444111111111111111111441111111144444441111111114444441111111111111111111111114444444411441444111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114444444441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114444444441111111111111111111114444444441111111111111111111111111111111111114411444414444444441111111111111111111111144   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�XA���A���A�dZA��A�dZA�;dA��7A��A�z�A�z�A�z�A�x�A�x�A�v�A�t�A�x�A�z�A�|�A�|�A��+A��A��7A��A��+A��PA��PA���A��A�A�wA�RA�FA�-A�RA�jA�ȴA�A�RA�A�jA�jA�RA���A��+A�ffA�=qA��A���A߰!A�n�A�-A��A޲-A�|�A�1'A��HA�C�A�&�A�VA՟�A�7LA� �A�"�A��A�33A�ZA�{A��A׮A�C�A�(�A�K�A�|�A��TA�?}A�n�AܑhAܡ�A�z�A�O�A�l�Aۇ+AܶFA�z�A��`AݾwA��A��AށAޅA�~�A�ȴA�bAʴ9A�1'A�x�Aȗ�A��A�?}A�ĜA��A�(�AԑhA��HA���Aԛ�AҼjA� �Aϥ�A��Aȧ�A�?}A�VA�VA�33A�S�A̾wA�A��A��A��A�$�A�+A�+A�&�A�"�A��A�bA��A�ƨA̗�A�XA�(�A��A˙�A�?}A�A�{A�$�A�I�A�C�A�XAŝ�A�7LA��Ać+A�bA�x�A�A�A�ĜA�9XA��`A�ȴA��A���A���A��A��A���A���A���A���A���A���A���A���A���A���A��A��wA��!A��!A��RA��wA��FA��A���A���A���A���A���A��!A��^A���A���A��-A��!A��-A��A��A��RA��wA�ƨA���A���A���A���A���A���A���A��/A��#A��A���A��
A���A���A���A���A���A�A��wA���A��/A��yA��A��A��yA��HA��HA���A���A�A�
=A�JA�bA��A��A�+A�-A�-A�1'A�C�A�C�A�C�A�A�A�?}A�S�A�\)A�\)A�\)A�\)A�ZA�ZA�`BA�ffA�hsA�n�A�v�A�x�A�|�AADAPAhAuAuA�A�A¡�A®A¸RAº^Aº^Aº^A���A���A��
A��#A��HA��yA��A���A�1A�JA��A��A��A��A��A�(�A�-A�/A�/A�5?A�;dA�C�A�M�A�K�A�K�A�Q�A�XA�XA�XA�XA�XA�ZA�`BA�dZA�n�A�r�A�x�A�z�AÁAÃAËDAÍPAÏ\AÕ�A×�AÛ�Aá�Aå�AîAþwAþwAþwA���A�A�ĜA���A���A��
A��A��/A��/A��/A��;A��;A��mA��A��A���A���A�A�A�1A��A��A��A��A�(�A�/A�/A�/A�/A�5?A�=qA�?}A�C�A�E�A�G�A�I�A�M�A�Q�A�S�A�VA�XA�XA�\)A�bNA�hsA�l�A�p�A�r�A�t�A�x�A�z�A�z�AāAć+Aĉ7AčPAčPAď\AđhAđhAĕ�Aě�Aě�Ağ�Aĥ�Aĩ�AĮAİ!Aİ!AĲ-AĴ9AĶFAĸRAĺ^AļjAľwA���A�A�ƨA�ȴA���A�ȴA���A���A���A���A���A��
A��A��#A��;A��HA��HA��`A��`A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A�  A�A�A�A�%A�%A�1A�
=A�JA�JA�VA�VA�VA�VA�bA�bA�oA�oA�{A�{A��A��A��A��A��A��A��A��A� �A� �A�"�A�"�A�&�A�+A�+A�-A�/A�1'A�33A�33A�5?A�5?A�5?A�5?A�7LA�9XA�9XA�9XA�9XA�;dA�?}A�?}A�A�A�A�A�C�A�C�A�C�A�C�A�E�A�G�A�I�A�I�A�K�A�K�A�M�A�M�A�O�A�Q�A�Q�A�S�A�VA�VA�\)A�dZA�r�A�z�Aŉ7A��`A�1AƇ+A�JA��A���A��/A��AҮA��
A֑hA���AظRA�v�A��#A�VAٸRAضFA֍PA�ZA�AǓuA���A���AžwAŅA�jA�bNA�bNA�dZA�hsA�hsA�hsA�jA�jA�hsA�jA�jA�jA�jA�jA�jA�l�A�l�A�n�A�n�A�n�A�n�A�p�A�r�A�r�A�t�A�t�A�v�A�t�A�t�A�v�A�v�A�z�A�z�A�z�A�z�A�z�A�z�A�|�A�~�AŁAŃAŃAŃAŃAŃAŃAŃAŅAŅAŇ+Aŉ7Aŉ7Aŉ7Aŉ7Aŉ7AŇ+AŇ+AŇ+Aŉ7AŋDAŋDAŋDAŏ\Aŏ\Aŏ\AőhAőhAœuAŕ�Aŕ�Aŕ�Aŕ�Aŕ�Aŗ�Aŕ�Aŗ�Ař�Ař�Aś�Aś�Ař�Aŝ�Aş�Aš�Aţ�Ať�Aţ�Ať�Ať�Ať�Aŧ�Aŧ�Aŧ�Aũ�Aũ�AŬAŬAŬAŬAŬAŮAŮAŰ!AŰ!AŰ!AŰ!AŰ!AŲ-AŲ-AŴ9AŴ9AŶFAŶFAŶFAŶFAŸRAŸRAŸRAź^Aź^AŸRAżjAżjAżjAżjAžwAžwA���A���A���A�A�A�A�ĜA�ĜA�ĜA�ĜA�ƨA�ƨA�ƨA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��
A��A��A��A��A��A��A��A��A��A��A��A��#A��#A��#A��/A��/A��/A��/A��/A��/A��#A��A��/A��;A��;A��;A��;A��;A��;A��;A��;A��HA��HA��HA��TA��TA��`A��`A��`A��yA��mA��yA��yA��yA��yA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�  A�A�A�A�A�A�%A�%A�%A�%A�%A�%A�1A�
=A�
=A�
=A�
=A�
=A�JA�
=A�
=A�JA�
=A�JA�JA�VA�bA�oA�oA�{A�{A�{A�{A��A��A�&�A�7LA�?}A�G�A�M�A�XA�VA�XA�\)A�n�AƩ�A�  A��A�$�A��A��yA��/A���A���Aƺ^A���A��A�7LAǇ+AǋDA���A�ffA̙�Aϥ�A��A��AցA�/A�Q�A�\)A�I�A��`A�`BA�ƨA�5?A�~�AݶFA���A�"�A�oA��mAݴ9A݉7A�^5A�G�A�E�A�dZAݗ�A��A�/A�n�Aޗ�Aޣ�Aޝ�Aޗ�AށA�t�AެA�ƨA޲-AރA��A��yA��mAבhA�x�A�JA��A�VAƴ9A�n�AҲ-A��A؃A�ffA۾wA��A��/A�dZAީ�A�ȴA��TA��A�A�{A��A�$�A�$�A��A��yAޣ�A��A�"�A۟�A٩�A�jA�5?A���AƸRAƑhAƛ�A���A���A���A�33A��A�I�A�bA�Q�A�A��;A�jA���A��A�l�Aܟ�A�ȴAܶFA���A���A��;A��A��A��`A��HA��`A��A���A�  A��A�ĜA�~�A�=qA�A�A�z�A��A��mA��
AڶFA�^5A�
=Aى7A�VA�t�A��A��mA��`A��
AΥ�A�?}A��/A�p�A�I�A�33A�5?A�7LA�7LA�7LA�7LA�7LA�7LA�9XA�9XA�9XA�;dA�9XA�7LA�9XA�9XA�9XA�9XA�9XA�9XA�;dA�;dA�;dA��T44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   A�XA���A���A�dZA��A�dZA�;dA��7A��A�z�A�z�A�z�A�x�A�x�A�v�A�t�A�x�A�z�A�|�A�|�A��+A��A��7A��A��+A��PA��PA���A��A�A�wA�RA�FA�-A�RA�jA�ȴA�A�RA�A�jA�jA�RA���A��+A�ffA�=qA��A���A߰!A�n�A�-A��A޲-A�|�A�1'A��HA�C�A�&�A�VA՟�A�7LA� �A�"�A��A�33A�ZA�{A��A׮A�C�A�(�A�K�A�|�A��TA�?}A�n�AܑhAܡ�A�z�A�O�A�l�Aۇ+AܶFA�z�A��`AݾwA��A��AށAޅA�~�A�ȴA�bAʴ9A�1'A�x�Aȗ�A��A�?}A�ĜA��A�(�AԑhA��HA���Aԛ�AҼjA� �Aϥ�A��Aȧ�A�?}A�VA�VA�33A�S�A̾wA�A��A��A��A�$�A�+A�+A�&�A�"�A��A�bA��A�ƨA̗�A�XA�(�A��A˙�A�?}A�A�{A�$�A�I�A�C�A�XAŝ�A�7LA��Ać+A�bA�x�A�A�A�ĜA�9XA��`A�ȴA��A���A���A��A��A���A���A���A���A���A���A���A���A���A���A��A��wA��!A��!A��RA��wA��FA��A���A���A���A���A���A��!A��^A���A���A��-A��!A��-A��A��A��RA��wA�ƨA���A���A���A���A���A���A���A��/A��#A��A���A��
A���A���A���A���A���A�A��wA���A��/A��yA��A��A��yA��HA��HA���A���A�A�
=A�JA�bA��A��A�+A�-A�-A�1'A�C�A�C�A�C�A�A�A�?}A�S�A�\)A�\)A�\)A�\)A�ZA�ZA�`BA�ffA�hsA�n�A�v�A�x�A�|�AADAPAhAuAuA�A�A¡�A®A¸RAº^Aº^Aº^A���A���A��
A��#A��HA��yA��A���A�1A�JA��A��A��A��A��A�(�A�-A�/A�/A�5?A�;dA�C�A�M�A�K�A�K�A�Q�A�XA�XA�XA�XA�XA�ZA�`BA�dZA�n�A�r�A�x�A�z�AÁAÃAËDAÍPAÏ\AÕ�A×�AÛ�Aá�Aå�AîAþwAþwAþwA���A�A�ĜA���A���A��
A��A��/A��/A��/A��;A��;A��mA��A��A���A���A�A�A�1A��A��A��A��A�(�A�/A�/A�/A�/A�5?A�=qA�?}A�C�A�E�A�G�A�I�A�M�A�Q�A�S�A�VA�XA�XA�\)A�bNA�hsA�l�A�p�A�r�A�t�A�x�A�z�A�z�AāAć+Aĉ7AčPAčPAď\AđhAđhAĕ�Aě�Aě�Ağ�Aĥ�Aĩ�AĮAİ!Aİ!AĲ-AĴ9AĶFAĸRAĺ^AļjAľwA���A�A�ƨA�ȴA���A�ȴA���A���A���A���A���A��
A��A��#A��;A��HA��HA��`A��`A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A�  A�A�A�A�%A�%A�1A�
=A�JA�JA�VA�VA�VA�VA�bA�bA�oA�oA�{A�{A��A��A��A��A��A��A��A��A� �A� �A�"�A�"�A�&�A�+A�+A�-A�/A�1'A�33A�33A�5?A�5?A�5?A�5?A�7LA�9XA�9XA�9XA�9XA�;dA�?}A�?}A�A�A�A�A�C�A�C�A�C�A�C�A�E�A�G�A�I�A�I�A�K�A�K�A�M�A�M�A�O�A�Q�A�Q�A�S�A�VA�VA�\)A�dZA�r�A�z�Aŉ7A��`A�1AƇ+A�JA��A���A��/A��AҮA��
A֑hA���AظRA�v�A��#A�VAٸRAضFA֍PA�ZA�AǓuA���A���AžwAŅA�jA�bNA�bNA�dZA�hsA�hsA�hsA�jA�jA�hsA�jA�jA�jA�jA�jA�jA�l�A�l�A�n�A�n�A�n�A�n�A�p�A�r�A�r�A�t�A�t�A�v�A�t�A�t�A�v�A�v�A�z�A�z�A�z�A�z�A�z�A�z�A�|�A�~�AŁAŃAŃAŃAŃAŃAŃAŃAŅAŅAŇ+Aŉ7Aŉ7Aŉ7Aŉ7Aŉ7AŇ+AŇ+AŇ+Aŉ7AŋDAŋDAŋDAŏ\Aŏ\Aŏ\AőhAőhAœuAŕ�Aŕ�Aŕ�Aŕ�Aŕ�Aŗ�Aŕ�Aŗ�Ař�Ař�Aś�Aś�Ař�Aŝ�Aş�Aš�Aţ�Ať�Aţ�Ať�Ať�Ať�Aŧ�Aŧ�Aŧ�Aũ�Aũ�AŬAŬAŬAŬAŬAŮAŮAŰ!AŰ!AŰ!AŰ!AŰ!AŲ-AŲ-AŴ9AŴ9AŶFAŶFAŶFAŶFAŸRAŸRAŸRAź^Aź^AŸRAżjAżjAżjAżjAžwAžwA���A���A���A�A�A�A�ĜA�ĜA�ĜA�ĜA�ƨA�ƨA�ƨA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��
A��A��A��A��A��A��A��A��A��A��A��A��#A��#A��#A��/A��/A��/A��/A��/A��/A��#A��A��/A��;A��;A��;A��;A��;A��;A��;A��;A��HA��HA��HA��TA��TA��`A��`A��`A��yA��mA��yA��yA��yA��yA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�  A�A�A�A�A�A�%A�%A�%A�%A�%A�%A�1A�
=A�
=A�
=A�
=A�
=A�JA�
=A�
=A�JA�
=A�JA�JA�VA�bA�oA�oA�{A�{A�{A�{A��A��A�&�A�7LA�?}A�G�A�M�A�XA�VA�XA�\)A�n�AƩ�A�  A��A�$�A��A��yA��/A���A���Aƺ^A���A��A�7LAǇ+AǋDA���A�ffA̙�Aϥ�A��A��AցA�/A�Q�A�\)A�I�A��`A�`BA�ƨA�5?A�~�AݶFA���A�"�A�oA��mAݴ9A݉7A�^5A�G�A�E�A�dZAݗ�A��A�/A�n�Aޗ�Aޣ�Aޝ�Aޗ�AށA�t�AެA�ƨA޲-AރA��A��yA��mAבhA�x�A�JA��A�VAƴ9A�n�AҲ-A��A؃A�ffA۾wA��A��/A�dZAީ�A�ȴA��TA��A�A�{A��A�$�A�$�A��A��yAޣ�A��A�"�A۟�A٩�A�jA�5?A���AƸRAƑhAƛ�A���A���A���A�33A��A�I�A�bA�Q�A�A��;A�jA���A��A�l�Aܟ�A�ȴAܶFA���A���A��;A��A��A��`A��HA��`A��A���A�  A��A�ĜA�~�A�=qA�A�A�z�A��A��mA��
AڶFA�^5A�
=Aى7A�VA�t�A��A��mA��`A��
AΥ�A�?}A��/A�p�A�I�A�33A�5?A�7LA�7LA�7LA�7LA�7LA�7LA�9XA�9XA�9XA�;dA�9XA�7LA�9XA�9XA�9XA�9XA�9XA�9XA�;dA�;dA�;dA��T44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.08 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220220080144                              AO  ARCAADJP                                                                    20220220080144    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220220080144  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220220080144  QCF$                G�O�G�O�G�O�C000            