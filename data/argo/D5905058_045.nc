CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-03-10T18:35:32Z creation;2018-03-10T18:35:35Z conversion to V3.1;2019-12-23T06:25:39Z update;     
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �`   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �h   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �p   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180310183532  20200120021522  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               -A   JA  I2_0675_045                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�Rg�� 1   @�Rho� @6�.��2��b��Mj1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DLfDL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�3D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�Ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�(�@ʏ\AG�A%G�AEG�AeG�A���A���A���A���A£�Aң�A��A��BQ�B	Q�BQ�BQ�B!Q�B)Q�B1Q�B9Q�BAQ�BIQ�BQQ�BYQ�BaQ�BiQ�BqQ�ByQ�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĨ�BȨ�B̨�BШ�BԨ�Bب�Bܨ�B��B��B��B��B��B���B���B���C T{CT{CT{CT{CT{C
T{CT{CT{CT{CT{CT{CT{CT{CT{CT{CT{C T{C"T{C$T{C&T{C(T{C*T{C,T{C.T{C0T{C2T{C4T{C6T{C8T{C:T{C<T{C>T{C@nCBT{CDT{CFT{CHT{CJT{CLT{CNT{CPT{CRT{CTT{CVT{CXT{CZT{C\T{C^T{C`T{CbT{CdT{CfT{ChT{CjT{ClT{CnT{CpT{CrT{CtT{CvT{CxT{CzT{C|T{C~T{C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DL�DL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�DuDu�DvDv�DwDw�DxDx�DyDy�DzDz�D{D{�D|D|�D}D}�D~D~�DD�D�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D��\D��\D�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D�D�ʏD�
�D�J�DÊ�D�ʏD�
�D�J�DĊ�D�ʏD�
�D�J�DŊ�D�ʏD�
�D�J�DƊ�D�ʏD�
�D�J�DǊ�D�ʏD��D�J�DȊ�D�ʏD�
�D�J�DɊ�D�ʏD�
�D�J�Dʊ�D�ʏD�
�D�J�Dˊ�D�ʏD�
�D�J�D̊�D�ʏD�
�D�J�D͊�D�ʏD�
�D�J�DΊ�D�ʏD�
�D�J�Dϊ�D�ʏD�
�D�J�DЊ�D�ʏD�
�D�J�Dъ�D�ʏD�
�D�J�DҊ�D�ʏD�
�D�J�Dӊ�D�ʏD�
�D�J�DԊ�D�ʏD�
�D�J�DՊ�D�ʏD�
�D�J�D֊�D�ʏD�
�D�J�D׊�D�ʏD�
�D�J�D؊�D�ʏD�
�D�J�Dي�D�ʏD�
�D�J�Dڊ�D�ʏD�
�D�J�Dۊ�D�ʏD�
�D�J�D܊�D�ʏD�
�D�J�D݊�D�ʏD�
�D�J�Dފ�D�ʏD�
�D�J�Dߊ�D�ʏD�
�D�J�D���D�ʏD�
�D�J�D኏D�ʏD�
�D�J�D⊏D�ʏD�
�D�J�D㊏D�ʏD�
�D�J�D䊏D�ʏD�
�D�J�D劏D�ʏD�
�D�J�D抏D�ʏD�
�D�J�D犏D�ʏD�
�D�J�D芏D�ʏD�
�D�J�D銏D�ʏD�
�D�J�DꊏD�ʏD�
�D�J�D��D�ʏD�
�D�J�D슏D�ʏD�
�D�J�D튏D�ʏD�
�D�J�DD�ʏD�
�D�J�DD�ʏD�
�D�J�D���D�ʏD�
�D�J�D�D�ʏD�
�D�J�D�D�ʏD�
�D�J�D�D�ʏD�
�D�J�D�D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D���D�
�D�J�D���D�ʏD�
�D�P�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A��hA��uA���A��9A��9A���A���A��A��RA��!A���A��\A�n�A�?}A�%A�|�A��A�ĜA�A�A��A�A���A��#A��jA��A���A���A���A���A���A���A���A���A��hA��\A��7A��+A��A�|�A�t�A�r�A�t�A�bNA�I�A�+A��HA���A�bA�l�A�x�A�dZA���A�l�A�z�A��A��FA� �A�~�A��A�%A��A���A��uA�%A��uA�C�A���A���A�
=A���A�|�A��+A���A���A�$�A��yA���A���A�?}A���A��PA�XA��A��A��hA���A�`BA���A� �A���A�n�A�JA���A��A��jA��uA��A���A��A��`A���A���A��mA���A�7LA{��Ax��As\)Aq`BAm��Al��AjVAg��Ae�7Ae��Aet�Ad��Ad(�AbA_G�A_K�A^��AZ^5AW`BASG�AP��AM�AMXAK�AHI�AF  AD{ABA�A?�wA>$�A<�/A:$�A8Q�A6z�A4�A3��A1��A1oA0�A.n�A,�+A,  A++A*ZA)��A(��A'�TA'"�A&��A&�uA&ZA%�-A#�wA"��A"�A r�AdZA9XA��A��AdZA&�A��A~�A=qA�A`BA�A�jA�hA�DAt�AK�A;dA��Ax�A��A��A�A9XA�At�A/A�AhsA
��A	��A	;dA�A�A��AĜA��AI�A��A��A�jAZA-A��AC�A{A+A A�@�v�@�?}@�r�@�M�@���@�hs@���@�K�@��@��@���@�(�@�M�@���@�~�@��@���@�@��@�/@�@�S�@�@���@�~�@�7L@ܼj@�I�@�^5@١�@�7L@؃@�Q�@ו�@�?}@�v�@��`@ϝ�@ͺ^@�9X@�b@���@�dZ@�J@�?}@ț�@���@�o@���@Ɵ�@Ƈ+@őh@ă@�@�V@���@�\)@�O�@���@�K�@��R@�-@��T@�@��7@�I�@���@�=q@�5?@�$�@��@�x�@�b@��w@�ƨ@���@�v�@���@���@��u@��j@��u@�1@�dZ@�C�@���@���@���@��@�(�@���@�S�@��!@�V@���@�p�@�&�@���@�Z@�I�@�  @��P@��H@�E�@���@��@�`B@���@��u@�1@��w@��P@�l�@�+@��y@���@�n�@�E�@��-@���@��j@���@�;d@��H@�$�@��@���@�7L@��j@�j@�Q�@�I�@�I�@�(�@���@��w@�l�@�;d@�ȴ@�ff@�5?@��@���@��^@��^@��-@��-@��h@�p�@�7L@��9@��u@�r�@�9X@�1@�ƨ@��@��y@�ȴ@���@���@�~�@�ff@�E�@�=q@�E�@�E�@�=q@�-@�{@��T@�@��-@���@��h@��@�p�@�%@��j@��u@��@�Z@�b@��;@�ƨ@��F@���@���@�|�@�dZ@�33@���@�v�@��@��@��T@�@�p�@��@���@��j@�bN@��w@�|�@�l�@�\)@�33@�o@��H@���@�V@�@���@���@��h@�X@��@�%@��/@�r�@��@��;@���@��@�dZ@�"�@��@�n�@��@��T@��-@�x�@�O�@���@�Ĝ@��@�Z@�(�@�1@��
@���@��@�\)@�o@�@��y@��!@��\@�-@�$�@�-@�@��@��@���@��@��T@�@��7@�&�@��/@��@��9@���@���@��D@�I�@��@��m@��w@��F@�|�@�
=@���@���@���@��y@��\@�M�@��@���@��@�$�@�=q@�{@�@��h@�x�@�X@�/@���@���@��@�Z@�(�@�;@��@l�@;d@
=@~�@~E�@}�T@}�@|��@|�j@|I�@{�
@{�@{33@z��@z^5@zM�@y��@y&�@x��@xr�@x1'@x �@x  @w�@w��@wl�@w+@v�@v��@v��@vv�@v5?@u�@u�@u`B@u?}@t��@tI�@t1@s�
@s�F@s33@r�H@r�\@r=q@q��@q��@qX@q%@p��@p�9@pQ�@p1'@p �@o�;@oK�@nff@m@l�@l1@kƨ@k��@kdZ@j�@i�^@i%@h�9@h�u@h�@hQ�@h �@g�@g+@fE�@f@e�@e�-@ep�@d�@dI�@d1@cƨ@c�@cC�@b��@b�@ax�@a%@`��@`�9@`�u@`r�@`1'@`  @_��@_�P@_K�@_;d@^�@^ff@^{@]��@]�h@]O�@]V@\�@\�@\9X@\�@[�F@[S�@Z=q@Y��@Y��@Yhs@Yhs@Yhs@X��@X�u@X�@Xr�@XbN@X �@W��@WK�@W�@V�@V$�@V@V@U�T@U�@UO�@Tj@Sƨ@S@Rn�@Q�@Q��@Q�7@Q7L@PĜ@P�9@Pr�@P  @O�w@O\)@N��@N�@N��@M�@M��@M/@L�/@L�j@L�@L��@L�D@L9X@L1@L1@K��@KS�@J�@J��@I�#@I��@I�7@H��@HbN@H �@G�w@G|�@G;d@F�@F�R@F�R@F�R@F�R@F��@F�+@Fv�@FV@F$�@E��@E�-@Ep�@D��@D�/@D�@D9X@D�@D1@C��@CS�@CS�@Co@B��@B�\@B�@A��@Ax�@AX@A%@@��@@�@@b@?��@?\)@?+@>�y@>�@>�R@>v�@>@=�@<�@<�j@<��@<�D@<z�@<�@;��@;33@:�@:�H@:�!@:n�@9�#@9hs@9G�@9%@8��@8�9@8�u@8r�@8b@7�@7�@7�@7�;@7��@7�P@7K�@7
=@6��@65?@6@5��@5�h@5p�@5O�@4�/@4��@4Z@4(�@4�@41@3�m@3�
@3��@3C�@3"�@2��@2=q@1�#@1�^@1�7@1hs@1X@1&�@0��@0Ĝ@0��@0bN@0  @/�P@.��@.�@.ȴ@.�R@.��@.�+@.�+@.�+@.E�@-@-�@-O�@-/@-V@,��@,j@,�@+�F@+C�@+@*^5@*�@)��@)��@)�7@)X@(�`@(��@(�9@(�u@(�@(bN@(1'@( �@(b@'�w@'�@'��@'+@&v�@&5?@%�@%��@%@%��@%/@$�D@$j@$I�@$1@#��@#S�@#"�@"�H@"��@"�\@"n�@"n�@"-@!��@!x�@!7L@!%@ �`@ �9@ �@ A�@  �@   @�;@�;@�@+@
=@�y@�@��@ff@V@E�@@@�@�h@O�@`B@O�@��@�@��@t�@dZ@C�@33@"�@o@�H@�!@��@^5@J@�#@��@��@��@�7@X@G�@&�@%@Ĝ@��@r�@Q�@1'@�;@l�@;d@�@�y@�R@�+@V@@�T@��@��@p�@`B@O�@/@��@�@�@�D@9X@1@�m@�
@��@dZ@33@o@@�@�H@��@�\@^5@�#@��@x�@G�@�@%@%@Ĝ@�9@��@Q�@ �@b@�@�;@��@�@�P@|�@l�@\)@l�@\)@K�@
=@��@�R@ff@$�@{@�@�-@p�@?}@�@V@�/@�@j@I�@(�@1@��@�
@��@�@t�@S�@@
��@
��@
��@
M�@	�#@	��@	�^@	��@	hs@	G�@	%@�9@r�@A�@ �@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A��hA��uA���A��9A��9A���A���A��A��RA��!A���A��\A�n�A�?}A�%A�|�A��A�ĜA�A�A��A�A���A��#A��jA��A���A���A���A���A���A���A���A���A��hA��\A��7A��+A��A�|�A�t�A�r�A�t�A�bNA�I�A�+A��HA���A�bA�l�A�x�A�dZA���A�l�A�z�A��A��FA� �A�~�A��A�%A��A���A��uA�%A��uA�C�A���A���A�
=A���A�|�A��+A���A���A�$�A��yA���A���A�?}A���A��PA�XA��A��A��hA���A�`BA���A� �A���A�n�A�JA���A��A��jA��uA��A���A��A��`A���A���A��mA���A�7LA{��Ax��As\)Aq`BAm��Al��AjVAg��Ae�7Ae��Aet�Ad��Ad(�AbA_G�A_K�A^��AZ^5AW`BASG�AP��AM�AMXAK�AHI�AF  AD{ABA�A?�wA>$�A<�/A:$�A8Q�A6z�A4�A3��A1��A1oA0�A.n�A,�+A,  A++A*ZA)��A(��A'�TA'"�A&��A&�uA&ZA%�-A#�wA"��A"�A r�AdZA9XA��A��AdZA&�A��A~�A=qA�A`BA�A�jA�hA�DAt�AK�A;dA��Ax�A��A��A�A9XA�At�A/A�AhsA
��A	��A	;dA�A�A��AĜA��AI�A��A��A�jAZA-A��AC�A{A+A A�@�v�@�?}@�r�@�M�@���@�hs@���@�K�@��@��@���@�(�@�M�@���@�~�@��@���@�@��@�/@�@�S�@�@���@�~�@�7L@ܼj@�I�@�^5@١�@�7L@؃@�Q�@ו�@�?}@�v�@��`@ϝ�@ͺ^@�9X@�b@���@�dZ@�J@�?}@ț�@���@�o@���@Ɵ�@Ƈ+@őh@ă@�@�V@���@�\)@�O�@���@�K�@��R@�-@��T@�@��7@�I�@���@�=q@�5?@�$�@��@�x�@�b@��w@�ƨ@���@�v�@���@���@��u@��j@��u@�1@�dZ@�C�@���@���@���@��@�(�@���@�S�@��!@�V@���@�p�@�&�@���@�Z@�I�@�  @��P@��H@�E�@���@��@�`B@���@��u@�1@��w@��P@�l�@�+@��y@���@�n�@�E�@��-@���@��j@���@�;d@��H@�$�@��@���@�7L@��j@�j@�Q�@�I�@�I�@�(�@���@��w@�l�@�;d@�ȴ@�ff@�5?@��@���@��^@��^@��-@��-@��h@�p�@�7L@��9@��u@�r�@�9X@�1@�ƨ@��@��y@�ȴ@���@���@�~�@�ff@�E�@�=q@�E�@�E�@�=q@�-@�{@��T@�@��-@���@��h@��@�p�@�%@��j@��u@��@�Z@�b@��;@�ƨ@��F@���@���@�|�@�dZ@�33@���@�v�@��@��@��T@�@�p�@��@���@��j@�bN@��w@�|�@�l�@�\)@�33@�o@��H@���@�V@�@���@���@��h@�X@��@�%@��/@�r�@��@��;@���@��@�dZ@�"�@��@�n�@��@��T@��-@�x�@�O�@���@�Ĝ@��@�Z@�(�@�1@��
@���@��@�\)@�o@�@��y@��!@��\@�-@�$�@�-@�@��@��@���@��@��T@�@��7@�&�@��/@��@��9@���@���@��D@�I�@��@��m@��w@��F@�|�@�
=@���@���@���@��y@��\@�M�@��@���@��@�$�@�=q@�{@�@��h@�x�@�X@�/@���@���@��@�Z@�(�@�;@��@l�@;d@
=@~�@~E�@}�T@}�@|��@|�j@|I�@{�
@{�@{33@z��@z^5@zM�@y��@y&�@x��@xr�@x1'@x �@x  @w�@w��@wl�@w+@v�@v��@v��@vv�@v5?@u�@u�@u`B@u?}@t��@tI�@t1@s�
@s�F@s33@r�H@r�\@r=q@q��@q��@qX@q%@p��@p�9@pQ�@p1'@p �@o�;@oK�@nff@m@l�@l1@kƨ@k��@kdZ@j�@i�^@i%@h�9@h�u@h�@hQ�@h �@g�@g+@fE�@f@e�@e�-@ep�@d�@dI�@d1@cƨ@c�@cC�@b��@b�@ax�@a%@`��@`�9@`�u@`r�@`1'@`  @_��@_�P@_K�@_;d@^�@^ff@^{@]��@]�h@]O�@]V@\�@\�@\9X@\�@[�F@[S�@Z=q@Y��@Y��@Yhs@Yhs@Yhs@X��@X�u@X�@Xr�@XbN@X �@W��@WK�@W�@V�@V$�@V@V@U�T@U�@UO�@Tj@Sƨ@S@Rn�@Q�@Q��@Q�7@Q7L@PĜ@P�9@Pr�@P  @O�w@O\)@N��@N�@N��@M�@M��@M/@L�/@L�j@L�@L��@L�D@L9X@L1@L1@K��@KS�@J�@J��@I�#@I��@I�7@H��@HbN@H �@G�w@G|�@G;d@F�@F�R@F�R@F�R@F�R@F��@F�+@Fv�@FV@F$�@E��@E�-@Ep�@D��@D�/@D�@D9X@D�@D1@C��@CS�@CS�@Co@B��@B�\@B�@A��@Ax�@AX@A%@@��@@�@@b@?��@?\)@?+@>�y@>�@>�R@>v�@>@=�@<�@<�j@<��@<�D@<z�@<�@;��@;33@:�@:�H@:�!@:n�@9�#@9hs@9G�@9%@8��@8�9@8�u@8r�@8b@7�@7�@7�@7�;@7��@7�P@7K�@7
=@6��@65?@6@5��@5�h@5p�@5O�@4�/@4��@4Z@4(�@4�@41@3�m@3�
@3��@3C�@3"�@2��@2=q@1�#@1�^@1�7@1hs@1X@1&�@0��@0Ĝ@0��@0bN@0  @/�P@.��@.�@.ȴ@.�R@.��@.�+@.�+@.�+@.E�@-@-�@-O�@-/@-V@,��@,j@,�@+�F@+C�@+@*^5@*�@)��@)��@)�7@)X@(�`@(��@(�9@(�u@(�@(bN@(1'@( �@(b@'�w@'�@'��@'+@&v�@&5?@%�@%��@%@%��@%/@$�D@$j@$I�@$1@#��@#S�@#"�@"�H@"��@"�\@"n�@"n�@"-@!��@!x�@!7L@!%@ �`@ �9@ �@ A�@  �@   @�;@�;@�@+@
=@�y@�@��@ff@V@E�@@@�@�h@O�@`B@O�@��@�@��@t�@dZ@C�@33@"�@o@�H@�!@��@^5@J@�#@��@��@��@�7@X@G�@&�@%@Ĝ@��@r�@Q�@1'@�;@l�@;d@�@�y@�R@�+@V@@�T@��@��@p�@`B@O�@/@��@�@�@�D@9X@1@�m@�
@��@dZ@33@o@@�@�H@��@�\@^5@�#@��@x�@G�@�@%@%@Ĝ@�9@��@Q�@ �@b@�@�;@��@�@�P@|�@l�@\)@l�@\)@K�@
=@��@�R@ff@$�@{@�@�-@p�@?}@�@V@�/@�@j@I�@(�@1@��@�
@��@�@t�@S�@@
��@
��@
��@
M�@	�#@	��@	�^@	��@	hs@	G�@	%@�9@r�@A�@ �@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B�B�-B�LB��B��B�B�B��BVB�B�B�B�B�B�B�B�B �B �B �B �B �B �B�B �B �B�B�B �B�B�B �B!�B%�B&�B)�B2-B33BH�Br�B�B~�B|�Bv�Bo�Bm�Bn�Bp�BffBG�B2-B2-B49B9XB%�B!�B�B{B	7BBB��B�B��BB�RB��B�hB�%By�Bn�B\)BD�B!�B
��B
�ZB
��B
��B
ŢB
�dB
�!B
��B
�7B
�B
|�B
u�B
s�B
m�B
P�B
;dB
/B
�B

=B
  B	��B	�B	ɺB	�!B	�B	v�B	S�B	G�B	;dB	>wB	B�B	P�B	P�B	L�B	G�B	=qB	,B	)�B	%�B	VB��B�BB��B�}B�^B�-B��B�hB�JB�Br�BjBdZBbNBq�Bw�Bq�Bo�Bn�BjBjBhsBaHB`BB_;B]/B\)BYBW
BVBT�BS�BR�BQ�BO�BL�BL�BH�BG�BE�BE�BC�BB�BB�BB�BA�B@�B>wB?}B;dB:^B:^B9XB6FB5?B49B49B2-B/B.B,B+B)�B'�B%�B%�B&�B#�B$�B!�B!�B"�B!�B!�B �B!�B!�B"�B#�B#�B$�B&�B&�B"�B �B!�B!�B"�B"�B$�B#�B"�B"�B#�B&�B(�B(�B'�B(�B(�B)�B)�B)�B(�B(�B(�B(�B(�B)�B)�B+B+B+B)�B+B(�B(�B(�B'�B'�B)�B+B)�B)�B,B,B,B+B+B.B/B0!B1'B33B49B49B33B6FB7LB8RB7LB7LB>wBD�BI�BL�BO�BS�BW
BW
BXBffBl�Bn�Bp�Br�Bu�Bx�Bx�Bz�B}�B�B�1B�=B�\B�bB�uB��B��B��B��B��B�B�!B�B�3B�FB�RB�jB�qB��BBŢBɺB��B��B��B��B�
B�#B�;B�;B�;B�BB�NB�`B�mB�sB�yB�B�B�B�B��B��B��B��B	  B	B	+B	PB	\B	oB	�B	�B	�B	�B	�B	�B	!�B	#�B	%�B	(�B	,B	0!B	33B	6FB	9XB	;dB	?}B	@�B	@�B	@�B	B�B	C�B	F�B	L�B	M�B	N�B	Q�B	S�B	W
B	^5B	`BB	aHB	cTB	cTB	e`B	gmB	hsB	iyB	iyB	hsB	iyB	jB	m�B	p�B	q�B	s�B	s�B	t�B	u�B	v�B	|�B	� B	�B	�B	�B	�%B	�7B	�=B	�DB	�JB	�JB	�VB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�?B	�LB	�XB	�^B	�dB	�qB	�wB	�wB	�}B	��B	ÖB	ĜB	ŢB	ŢB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�/B	�5B	�;B	�BB	�BB	�BB	�BB	�TB	�TB	�TB	�ZB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
1B
	7B

=B

=B

=B
DB
DB
DB
JB
JB
PB
PB
VB
VB
VB
VB
VB
VB
\B
\B
\B
bB
hB
oB
oB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
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
'�B
&�B
'�B
(�B
)�B
+B
+B
,B
,B
,B
-B
-B
.B
.B
/B
/B
/B
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
49B
49B
33B
49B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
8RB
9XB
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
<jB
<jB
<jB
<jB
=qB
<jB
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
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
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
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
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
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
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
W
B
XB
XB
XB
XB
YB
YB
YB
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
_;B
_;B
`BB
`BB
`BB
`BB
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
cTB
cTB
cTB
cTB
cTB
dZB
dZB
cTB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
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
iyB
jB
jB
jB
jB
jB
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
l�B
m�B
m�B
m�B
m�B
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
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B�B�2B�iBʦB��B�}B��B<B�B�B�B�B�B�B�B�B �B �B �B �B �B �B�B �B �B�B�B �B�B�B �B!�B%�B&�B)�B2B3BH�Br�B��B~�B|�Bv�Bo�BmwBn}Bp�BfLBG�B2B2B4B9>B%�B!�B�BFB	B�B�B��B��BʌB�[B�8B��B�NB�By�Bn}B\BD�B!�B
��B
�@B
��B
˒B
�mB
�JB
�B
��B
�B
��B
|�B
u�B
s�B
mwB
P�B
;0B
.�B
�B

#B	��B	��B	�}B	ɠB	�B	�B	v�B	S�B	G�B	;0B	>BB	B[B	P�B	P�B	L�B	GzB	=<B	+�B	)�B	%�B	"B��B�B��B�HB�DB��B��B�4B�0B��Br�BjKBd&Bb4BqvBw�Bq�Bo�BncBjKBjeBhXBa-B`B_!B\�B\BX�BV�BU�BT�BS�BR�BQ�BO�BL�BL�BH�BG�BEmBE�BC{BB[BBuBBuBAoB@OB>]B?HB;JB:*B:DB9>B6B5B4B4B2B/ B-�B+�B*�B)�B'�B%�B%�B&�B#�B$�B!�B!�B"�B!�B!�B �B!�B!�B"�B#�B#�B$�B&�B&�B"�B �B!�B!�B"�B"�B$�B#�B"�B"�B#�B&�B(�B(�B'�B(�B(�B)�B)�B)�B(�B(�B(�B(�B(�B)�B)�B*�B*�B*�B)�B*�B(�B(�B(�B'�B'�B)�B*�B)�B)�B+�B+�B+�B*�B*�B-�B.�B/�B0�B2�B4B4B2�B6+B72B8B72B7B>BBD�BI�BL�BO�BS�BV�BV�BW�Bf2BlWBncBpoBr�Bu�Bx�Bx�Bz�B}�B��B�B�	B�(B�.B�[B�SB��B��B��B��B��B�B��B�B�+B�8B�PB�<B�OB�[B�mBɆBΥB��BѷBҽB��B�	B�B�!B�!B�'B�B�,B�8B�>B�DB�QB�]B��B�B�B��B��B��B��B	�B	B	B	(B	:B	MB	�B	�B	�B	�B	�B	!�B	#�B	%�B	(�B	+�B	/�B	2�B	6+B	9>B	;0B	?HB	@iB	@iB	@OB	BuB	C{B	FtB	L�B	M�B	N�B	Q�B	S�B	V�B	^B	`B	aB	c B	c B	eFB	g8B	hXB	iDB	i_B	hXB	iDB	jeB	mwB	poB	q�B	s�B	s�B	t�B	u�B	v�B	|�B	�B	��B	��B	��B	�B	�B	�#B	�B	�0B	�B	�<B	�(B	�TB	�SB	�_B	�eB	��B	�xB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�$B	�*B	�0B	�<B	�BB	�BB	�HB	�oB	�{B	āB	�mB	�mB	�zB	ɆB	ɆB	˒B	͟B	͟B	οB	��B	��B	ѷB	ҽB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�B	�B	�B	�'B	�:B	� B	�:B	�@B	�8B	�8B	�>B	�_B	�eB	�KB	�kB	�kB	�QB	�kB	�eB	�QB	�]B	�B	�oB	�B	�B	�iB	�vB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
�B
�B
�B
B
B
	B

	B

#B

	B
)B
)B
B
0B
B
6B
B
"B
"B
"B
<B
<B
<B
BB
(B
(B
HB
4B
:B
TB
@B
FB
FB
FB
FB
FB
MB
SB
mB
SB
SB
SB
SB
YB
YB
eB
eB
B
eB
eB
�B
qB
kB
�B
�B
qB
�B
�B
~B
~B
~B
~B
�B
�B
~B
~B
~B
~B
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
 �B
!�B
!�B
!�B
!�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
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
'�B
&�B
'�B
(�B
)�B
*�B
*�B
+�B
+�B
+�B
,�B
,�B
-�B
-�B
.�B
/ B
.�B
.�B
/ B
.�B
/�B
/�B
1B
0�B
1B
0�B
0�B
1B
1�B
1�B
2B
1�B
2�B
2�B
2�B
4B
4B
2�B
4B
5B
5%B
5B
6B
6B
7B
7B
7B
72B
7B
7B
7B
7B
72B
72B
88B
8B
88B
9$B
8B
9>B
9>B
9$B
9$B
:*B
:*B
:DB
:*B
:*B
;JB
;JB
<6B
<6B
<PB
<6B
=<B
<PB
=<B
=VB
>BB
>BB
>]B
>BB
>BB
?cB
?HB
?cB
@OB
@OB
@OB
@iB
@OB
AUB
AUB
B[B
BuB
B[B
B[B
B[B
CaB
CaB
CaB
DgB
DgB
DgB
DgB
D�B
DgB
DgB
EmB
DgB
DgB
DgB
E�B
E�B
EmB
EmB
FtB
FtB
FtB
FtB
GzB
GzB
GzB
GzB
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
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
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
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
U�B
U�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
Y�B
ZB
ZB
Y�B
Y�B
[	B
[	B
[	B
Z�B
Z�B
Z�B
Z�B
\B
[�B
[�B
\B
\�B
\�B
\�B
\�B
\�B
\�B
]B
^B
^B
^B
^B
^B
_B
_B
_B
_B
_B
_!B
_B
`'B
`'B
`'B
`B
a-B
aB
aB
aB
a-B
a-B
bB
bB
b4B
bB
bB
c B
c B
c B
c:B
c B
d&B
d&B
c B
d@B
d@B
d@B
e,B
eFB
fLB
f2B
fLB
fLB
fLB
f2B
f2B
f2B
fLB
fLB
fLB
f2B
g8B
g8B
gRB
g8B
h>B
h>B
h>B
h>B
h>B
hXB
h>B
h>B
iDB
iDB
iDB
iDB
iDB
jKB
jKB
jeB
jKB
jKB
jKB
jeB
jKB
jKB
kkB
kQB
kkB
kQB
kkB
kQB
kkB
kQB
kQB
kQB
kkB
kQB
kQB
kkB
lWB
lWB
lqB
lWB
lqB
lWB
m]B
m]B
mwB
m]B
ncB
ncB
ncB
ncB
oiB
o�B
o�B
o�B
oiB
oiB
oiB
oiB
p�B
poB
p�B
p�B
qvB
qvB
q�B
qvB
qvB
q�B
r|B
r|B
r|B
r|B
s�B
s�B
s�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.33(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201803160042352018031600423520180316004235201804060311232018040603112320180406031123JA  ARFMdecpA19c                                                                20180311033512  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180310183532  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180310183533  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180310183533  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180310183534  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180310183534  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180310183534  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180310183534  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180310183534  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180310183535                      G�O�G�O�G�O�                JA  ARUP                                                                        20180310185638                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180312161213  CV  JULD            G�O�G�O�F>                JM  ARGQJMQC2.0                                                                 20180312161213  CV  JULD_LOCATION   G�O�G�O�FL                JM  ARGQJMQC2.0                                                                 20180312161213  CV  LATITUDE        G�O�G�O�A�x�                JM  ARCAJMQC2.0                                                                 20180315154235  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180315154235  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180405181123  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021522                      G�O�G�O�G�O�                