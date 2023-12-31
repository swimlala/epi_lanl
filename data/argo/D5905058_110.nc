CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-12-13T15:36:25Z creation;2018-12-13T15:36:28Z conversion to V3.1;2019-12-23T06:10:23Z update;     
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
resolution        =���   axis      Z        h  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  `$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  sh   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �$   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �(   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �,   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �0   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �p   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181213153625  20200120021520  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               nA   JA  I2_0675_110                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @ؗ�فT 1   @ؗ�1M��@7�z����c?\(�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�fC  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�3D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D���D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D��3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@ʏ\AG�A%G�AEG�Ac�A���A���A���A���A£�Aң�A��A��BQ�B	Q�BQ�BQ�B!Q�B)Q�B1Q�B9Q�BAQ�BIQ�BQ�RBYQ�BaQ�BiQ�BqQ�ByQ�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĨ�BȨ�B̨�BШ�BԨ�Bب�B��)B��B��B��B��B��B���B���B���C T{CT{CT{C:�CT{C
T{CT{CT{CT{CT{CT{CT{CT{CT{CT{CT{C T{C"T{C$T{C&T{C(T{C*T{C,T{C.T{C0T{C2T{C4T{C6T{C8T{C:T{C<T{C>T{C@T{CBT{CDT{CFT{CHT{CJT{CLT{CNT{CPT{CRT{CTT{CVT{CXT{CZT{C\T{C^T{C`T{CbT{CdT{CfT{ChT{CjT{ClT{CnT{CpT{CrT{CtT{CvT{CxT{CzT{C|T{C~T{C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�qC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�qC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=D D �DD�DD��DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�DuDu�DvDv�DwDw�DxDx�DyDy�DzDz�D{D{�D|D|�D}D}�D~D~�DD�D�
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
�D�J�D���D�ʏD��D�M�D���D�ʏD�
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
�D�G\D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D���D�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D�D�ʏD�
�D�J�DÊ�D�ʏD��D�J�DĊ�D�ʏD�
�D�J�DŊ�D�ʏD�
�D�J�DƊ�D�ʏD�\D�J�DǊ�D�ʏD�
�D�J�DȊ�D�ʏD�
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
�D�J�D늏D�ʏD�
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
�D�J�D���D��\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AɁAɓuAɑhAɏ\Aɏ\Aɏ\AɓuAɕ�Aɝ�Aɟ�Aɡ�Aɡ�Aɣ�Aɣ�Aɡ�Aɡ�Aɡ�Aɡ�Aɣ�Aɧ�Aɧ�Aɧ�Aɧ�Aɩ�Aɩ�AɬAɬAɬAɬAɥ�Aɝ�Aɟ�Aə�AɓuA�l�A���A�C�A�v�A���A��A�oA�A��!A�E�A�A�A��TA�G�A��A�Q�A�\)A�K�A�
=A�ȴA��A�=qA�ĜA���A��9A���A�XA�
=A�^5A��A�5?A��mA��7A���A��wA�33A��^A��A�JA��
A�9XA���A���A�A��PA��A���A�JA��\A�v�A��hA��+A�1A���A�
=A�`BA��TA��7A�~�A��`A��A�I�A�VA���A�ƨA��/A�7LA�ȴA�A�A�%A��A��9A��A�\)A��7A�ZA�oA�G�A���A��A|�RAzĜAy%Aw��Au��As�Aq+Ao�Ao+An1Al-AkXAj9XAg�Ae�Ac�mAb�Ab5?Aa%A_�7A]��A[�mAZ(�AY33AWG�AU�#AT�ARZAP��AOl�AM%AJ�yAI��AJ  AI�AH��AGƨAFJAD�ADbNAA��A@�+A>�+A=p�A<VA< �A;��A:�A:M�A9�FA8�A7VA6�9A5�PA3�A37LA2  A1S�A0�!A/�A-��A,�9A+��A*�DA)��A)O�A(Q�A'�A&�+A&n�A&(�A#�A!�#A!�PA!O�A �A �9A ^5AdZA�DA��AM�A�!A�TA�DA �A`BA5?A�^AXA^5Ap�AI�AhsA�A|�AbAA�;A33A
ZA	%AjA/A�+A�A�hAM�A�Av�A1'A$�A��A ��A ��A @�o@�{@�O�@��@���@���@�t�@���@�V@�1@���@���@�w@�v�@�Q�@�+@�%@��@��H@�O�@�K�@�E�@�h@� �@�$�@�&�@�I�@�ƨ@�@և+@�b@��@�hs@�|�@�M�@���@́@̣�@�ƨ@��@�E�@ȓu@�bN@�1'@���@Ƈ+@�@ě�@�I�@Õ�@���@�=q@��^@�Ĝ@���@�$�@��@��@���@�b@��@�%@�I�@��F@�33@��!@��+@�=q@�J@���@�7L@��@�Q�@���@�
=@��+@�{@��7@�X@��@��j@��@�Q�@�  @���@�|�@�33@��@��R@�^5@�5?@�J@���@�1'@�t�@�o@���@��R@��+@�$�@��^@�?}@��D@��@�l�@��@�v�@�=q@��^@�G�@�Ĝ@�j@�1@��w@���@�|�@��@��y@��R@�n�@��-@��@�/@��@��@��@��@���@��D@�Q�@� �@�1@��;@��@�S�@�+@�o@���@���@��@�?}@�%@��9@��@���@��@�o@��H@��!@�$�@���@��h@��@�z�@� �@�  @��
@��
@��m@�b@��@��F@�\)@�"�@���@�=q@�{@���@���@�@���@���@�X@�&�@��9@�Q�@���@�\)@��@��R@���@�=q@��@��7@�X@�/@�%@��@�Q�@�A�@��9@�1'@��F@���@�\)@�
=@���@���@��\@��+@��+@��\@�~�@�v�@�n�@�M�@�$�@��@��@��@�{@��#@��h@���@���@���@���@���@��@�X@��@�r�@���@�9X@�(�@��@��@�33@��!@���@�ff@�ff@�^5@�V@�@��@�x�@�hs@�hs@�O�@��@��`@���@�Z@�A�@�9X@�1'@�(�@� �@�b@�  @��
@�dZ@��@��y@��@���@���@���@��\@�~�@�ff@�=q@�{@��#@�X@�&�@�Ĝ@��@�j@�I�@�9X@��m@�S�@��@���@��+@�~�@�n�@��T@��@�X@��@��/@��@�Q�@�b@�;@|�@~��@~�@~�+@}�h@}�@|�D@|1@{�m@{S�@zn�@y��@y&�@x�u@xA�@xA�@x �@x �@x �@x �@x1'@x �@w�w@w;d@w�@v�@v�R@v��@vff@vV@u�@u@uV@t��@t��@tj@tI�@t(�@sS�@r�H@r��@r�!@r��@rJ@q�7@qx�@qG�@q�@p��@p��@pA�@o�;@o��@o��@o�@ol�@o\)@o+@o
=@n��@n�y@n�@nff@n{@m�T@m@m�@l9X@k�m@k�F@k��@kt�@kC�@j�H@jJ@i�#@ihs@h��@hQ�@h  @g�@g�P@g
=@f5?@e?}@eV@d�/@d�D@dj@c�m@c��@cdZ@cC�@co@b�!@bn�@b�@a�@a��@`�9@`�@`1'@_��@_K�@_�@^�y@^ȴ@^5?@]O�@]V@\�j@\j@[��@[S�@[33@[o@Zn�@Z=q@Y��@Y��@Y�7@Y�7@Yx�@YX@Y&�@X��@Xr�@XbN@X1'@W��@W�P@Wl�@Wl�@WK�@W+@W
=@V�R@VV@V@U�h@U/@T��@T��@T�D@TI�@T(�@S��@SS�@S"�@R�!@R~�@R-@Q�#@Q��@QX@P�`@P�9@PbN@Pb@O��@O��@O�w@O�@O��@Ol�@N�@Nff@N@M�T@M��@M/@L�/@L��@LZ@L�@K�
@KdZ@J�@I�@IG�@I%@H�9@Hr�@HQ�@Hb@G�@G�;@G��@G�@G��@G�P@F��@F��@Fff@F5?@F{@E@Ep�@E/@D�@D�j@D�D@DZ@D(�@C��@C�
@C�F@C�@B�@B~�@B=q@A�@A�7@A�7@A�7@Ax�@AG�@@�9@@  @?�w@?l�@>�y@>E�@=?}@=�@=�@=V@<�@<�@<�D@<(�@;�
@;�F@;��@;S�@:�@:��@:�\@:n�@:M�@:M�@:M�@:=q@:-@9��@9�@9��@9hs@9&�@8�`@8��@8Ĝ@8�u@8bN@8A�@7�@7|�@7;d@6ȴ@6v�@6$�@5�h@5O�@4��@4I�@4�@41@3ƨ@3��@3�@3�@3�@3S�@3"�@2��@2^5@1�^@1��@1��@17L@1&�@1&�@1�@0�u@/�;@/l�@/;d@/�@/
=@.�@.��@.ff@.5?@.5?@.$�@-�T@-p�@-?}@,��@,��@,j@+�m@+�F@+��@+��@+��@+�@+C�@+"�@+@*��@*~�@*=q@)�@)�7@)&�@(��@(Ĝ@(��@(�@(r�@(A�@'�@'�@'l�@';d@&�y@&��@&5?@%��@%��@%�@%O�@$�@$��@$(�@$1@$1@#ƨ@#��@#t�@#S�@#"�@"�@"�H@"��@"�!@"�\@"J@!��@!hs@!7L@!&�@!&�@!�@!%@ �`@ Ĝ@ �@ b@�@�P@|�@l�@K�@�@�y@�y@�y@ȴ@��@v�@v�@ff@$�@{@�T@@��@`B@O�@?}@/@�@��@j@I�@(�@��@�F@�@C�@"�@o@o@��@^5@��@hs@X@G�@G�@G�@7L@�@�`@�9@bN@A�@ �@�@�P@K�@+@�R@v�@E�@5?@5?@$�@{@�@�T@�T@��@`B@O�@��@��@z�@j@I�@1@ƨ@�F@��@S�@@�@�H@��@~�@=q@��@�#@��@��@X@�@��@Ĝ@�u@Q�@b@�@��@K�@;d@+@�@�y@ȴ@��@��@��@v�@ff@V@{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AɁAɓuAɑhAɏ\Aɏ\Aɏ\AɓuAɕ�Aɝ�Aɟ�Aɡ�Aɡ�Aɣ�Aɣ�Aɡ�Aɡ�Aɡ�Aɡ�Aɣ�Aɧ�Aɧ�Aɧ�Aɧ�Aɩ�Aɩ�AɬAɬAɬAɬAɥ�Aɝ�Aɟ�Aə�AɓuA�l�A���A�C�A�v�A���A��A�oA�A��!A�E�A�A�A��TA�G�A��A�Q�A�\)A�K�A�
=A�ȴA��A�=qA�ĜA���A��9A���A�XA�
=A�^5A��A�5?A��mA��7A���A��wA�33A��^A��A�JA��
A�9XA���A���A�A��PA��A���A�JA��\A�v�A��hA��+A�1A���A�
=A�`BA��TA��7A�~�A��`A��A�I�A�VA���A�ƨA��/A�7LA�ȴA�A�A�%A��A��9A��A�\)A��7A�ZA�oA�G�A���A��A|�RAzĜAy%Aw��Au��As�Aq+Ao�Ao+An1Al-AkXAj9XAg�Ae�Ac�mAb�Ab5?Aa%A_�7A]��A[�mAZ(�AY33AWG�AU�#AT�ARZAP��AOl�AM%AJ�yAI��AJ  AI�AH��AGƨAFJAD�ADbNAA��A@�+A>�+A=p�A<VA< �A;��A:�A:M�A9�FA8�A7VA6�9A5�PA3�A37LA2  A1S�A0�!A/�A-��A,�9A+��A*�DA)��A)O�A(Q�A'�A&�+A&n�A&(�A#�A!�#A!�PA!O�A �A �9A ^5AdZA�DA��AM�A�!A�TA�DA �A`BA5?A�^AXA^5Ap�AI�AhsA�A|�AbAA�;A33A
ZA	%AjA/A�+A�A�hAM�A�Av�A1'A$�A��A ��A ��A @�o@�{@�O�@��@���@���@�t�@���@�V@�1@���@���@�w@�v�@�Q�@�+@�%@��@��H@�O�@�K�@�E�@�h@� �@�$�@�&�@�I�@�ƨ@�@և+@�b@��@�hs@�|�@�M�@���@́@̣�@�ƨ@��@�E�@ȓu@�bN@�1'@���@Ƈ+@�@ě�@�I�@Õ�@���@�=q@��^@�Ĝ@���@�$�@��@��@���@�b@��@�%@�I�@��F@�33@��!@��+@�=q@�J@���@�7L@��@�Q�@���@�
=@��+@�{@��7@�X@��@��j@��@�Q�@�  @���@�|�@�33@��@��R@�^5@�5?@�J@���@�1'@�t�@�o@���@��R@��+@�$�@��^@�?}@��D@��@�l�@��@�v�@�=q@��^@�G�@�Ĝ@�j@�1@��w@���@�|�@��@��y@��R@�n�@��-@��@�/@��@��@��@��@���@��D@�Q�@� �@�1@��;@��@�S�@�+@�o@���@���@��@�?}@�%@��9@��@���@��@�o@��H@��!@�$�@���@��h@��@�z�@� �@�  @��
@��
@��m@�b@��@��F@�\)@�"�@���@�=q@�{@���@���@�@���@���@�X@�&�@��9@�Q�@���@�\)@��@��R@���@�=q@��@��7@�X@�/@�%@��@�Q�@�A�@��9@�1'@��F@���@�\)@�
=@���@���@��\@��+@��+@��\@�~�@�v�@�n�@�M�@�$�@��@��@��@�{@��#@��h@���@���@���@���@���@��@�X@��@�r�@���@�9X@�(�@��@��@�33@��!@���@�ff@�ff@�^5@�V@�@��@�x�@�hs@�hs@�O�@��@��`@���@�Z@�A�@�9X@�1'@�(�@� �@�b@�  @��
@�dZ@��@��y@��@���@���@���@��\@�~�@�ff@�=q@�{@��#@�X@�&�@�Ĝ@��@�j@�I�@�9X@��m@�S�@��@���@��+@�~�@�n�@��T@��@�X@��@��/@��@�Q�@�b@�;@|�@~��@~�@~�+@}�h@}�@|�D@|1@{�m@{S�@zn�@y��@y&�@x�u@xA�@xA�@x �@x �@x �@x �@x1'@x �@w�w@w;d@w�@v�@v�R@v��@vff@vV@u�@u@uV@t��@t��@tj@tI�@t(�@sS�@r�H@r��@r�!@r��@rJ@q�7@qx�@qG�@q�@p��@p��@pA�@o�;@o��@o��@o�@ol�@o\)@o+@o
=@n��@n�y@n�@nff@n{@m�T@m@m�@l9X@k�m@k�F@k��@kt�@kC�@j�H@jJ@i�#@ihs@h��@hQ�@h  @g�@g�P@g
=@f5?@e?}@eV@d�/@d�D@dj@c�m@c��@cdZ@cC�@co@b�!@bn�@b�@a�@a��@`�9@`�@`1'@_��@_K�@_�@^�y@^ȴ@^5?@]O�@]V@\�j@\j@[��@[S�@[33@[o@Zn�@Z=q@Y��@Y��@Y�7@Y�7@Yx�@YX@Y&�@X��@Xr�@XbN@X1'@W��@W�P@Wl�@Wl�@WK�@W+@W
=@V�R@VV@V@U�h@U/@T��@T��@T�D@TI�@T(�@S��@SS�@S"�@R�!@R~�@R-@Q�#@Q��@QX@P�`@P�9@PbN@Pb@O��@O��@O�w@O�@O��@Ol�@N�@Nff@N@M�T@M��@M/@L�/@L��@LZ@L�@K�
@KdZ@J�@I�@IG�@I%@H�9@Hr�@HQ�@Hb@G�@G�;@G��@G�@G��@G�P@F��@F��@Fff@F5?@F{@E@Ep�@E/@D�@D�j@D�D@DZ@D(�@C��@C�
@C�F@C�@B�@B~�@B=q@A�@A�7@A�7@A�7@Ax�@AG�@@�9@@  @?�w@?l�@>�y@>E�@=?}@=�@=�@=V@<�@<�@<�D@<(�@;�
@;�F@;��@;S�@:�@:��@:�\@:n�@:M�@:M�@:M�@:=q@:-@9��@9�@9��@9hs@9&�@8�`@8��@8Ĝ@8�u@8bN@8A�@7�@7|�@7;d@6ȴ@6v�@6$�@5�h@5O�@4��@4I�@4�@41@3ƨ@3��@3�@3�@3�@3S�@3"�@2��@2^5@1�^@1��@1��@17L@1&�@1&�@1�@0�u@/�;@/l�@/;d@/�@/
=@.�@.��@.ff@.5?@.5?@.$�@-�T@-p�@-?}@,��@,��@,j@+�m@+�F@+��@+��@+��@+�@+C�@+"�@+@*��@*~�@*=q@)�@)�7@)&�@(��@(Ĝ@(��@(�@(r�@(A�@'�@'�@'l�@';d@&�y@&��@&5?@%��@%��@%�@%O�@$�@$��@$(�@$1@$1@#ƨ@#��@#t�@#S�@#"�@"�@"�H@"��@"�!@"�\@"J@!��@!hs@!7L@!&�@!&�@!�@!%@ �`@ Ĝ@ �@ b@�@�P@|�@l�@K�@�@�y@�y@�y@ȴ@��@v�@v�@ff@$�@{@�T@@��@`B@O�@?}@/@�@��@j@I�@(�@��@�F@�@C�@"�@o@o@��@^5@��@hs@X@G�@G�@G�@7L@�@�`@�9@bN@A�@ �@�@�P@K�@+@�R@v�@E�@5?@5?@$�@{@�@�T@�T@��@`B@O�@��@��@z�@j@I�@1@ƨ@�F@��@S�@@�@�H@��@~�@=q@��@�#@��@��@X@�@��@Ĝ@�u@Q�@b@�@��@K�@;d@+@�@�y@ȴ@��@��@��@v�@ff@V@{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BT�BT�BT�BT�BT�BXBcTBp�B�B��B�yB�BB�HB�B��B��B"�B/B=qB?}BL�BW
BZB]/B`BBbNBu�B�JB�hB�hB�bB�1B�B�B� B|�B{�Bx�Bt�Bs�Br�Bl�BdZBcTB`BB[#BYBVBP�BL�BF�BC�B9XB.B'�B�BhB
=B��B�yB�qB��B�VB� Bp�BXB=qB6FB(�B�B\B1BB
�B
�BB
��B
ŢB
�RB
��B
��B
�{B
r�B
ffB
\)B
C�B
<jB
.B
$�B
�B
	7B	�B	�B	�BB	�B	��B	ǮB	B	�3B	��B	�oB	�=B	�%B	�B	v�B	l�B	aHB	N�B	J�B	;dB	1'B	)�B	�B	\B	
=B��B��B�B�B�B�B�B�B�B�B�TB�)B��B��B��BȴBƨBĜB��B�wB�XB�-B�B��B��B��B��B��B��B�{B�DB�B�B� B|�B{�By�Bz�Bw�Bv�Bt�Bt�BjBhsBgmBgmBe`Be`BbNB_;B]/BYBT�BQ�BM�BI�BG�BF�BE�BD�BE�BE�BB�B?}B=qB<jB8RB6FB49B2-B1'B/B-B-B)�B)�B(�B(�B'�B&�B$�B$�B$�B$�B#�B#�B!�B!�B �B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B"�B#�B%�B&�B%�B%�B#�B%�B%�B$�B&�B)�B(�B-B/B1'B5?B7LB9XB;dB=qB=qB<jB<jB?}B@�BD�BD�BF�BH�BI�BJ�BL�BS�BS�BS�BS�BT�BYB]/B_;BaHBcTBe`BffBgmBhsBiyBjBm�Bm�Bn�Bp�Bu�Bz�B~�B�B�B�B�%B�+B�1B�7B�DB�DB�JB�PB�VB�\B�bB�bB�oB��B��B��B��B��B��B��B��B��B��B��B�B�-B�?B�FB�^B�qB��BĜBȴB��B��B��B��B��B��B�B�NB�`B�yB�yB�yB�B�B�B��B��B��B��B	B	B	1B		7B	JB	bB	�B	�B	�B	 �B	$�B	'�B	)�B	)�B	,B	,B	/B	33B	6FB	7LB	<jB	>wB	?}B	@�B	A�B	C�B	C�B	F�B	I�B	J�B	L�B	M�B	O�B	R�B	S�B	S�B	S�B	VB	W
B	ZB	`BB	cTB	ffB	gmB	jB	k�B	l�B	o�B	s�B	x�B	|�B	~�B	�B	�B	�B	�B	�%B	�%B	�7B	�=B	�=B	�PB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�3B	�3B	�9B	�LB	�RB	�XB	�jB	�qB	�wB	��B	��B	ÖB	ĜB	ƨB	ȴB	ȴB	ȴB	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�5B	�5B	�5B	�5B	�5B	�;B	�BB	�BB	�BB	�BB	�BB	�HB	�`B	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
1B
1B
1B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B
DB
DB
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
VB
VB
VB
VB
VB
\B
hB
hB
hB
oB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
,B
,B
-B
,B
,B
,B
-B
-B
-B
.B
.B
.B
.B
/B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
33B
33B
33B
49B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
<jB
=qB
=qB
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
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
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
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
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
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
J�B
K�B
K�B
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
M�B
M�B
N�B
N�B
N�B
O�B
O�B
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
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
VB
W
B
W
B
XB
XB
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
ZB
ZB
[#B
[#B
[#B
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
\)B
\)B
\)B
\)B
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
^5B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
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
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
gmB
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
k�B
k�B
k�B
l�B
l�B
l�B
l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BT�BT�BT�BT�BT�BW�Bc:Bp�B��B��B�_B�'B�-B��B��B��B"�B/ B=VB?cBL�BV�BZB]B`'Bb4Bu�B�0B�NB�NB�HB�B��B��B�B|�B{�Bx�Bt�Bs�Br�BlqBd@Bc:B`'B[	BX�BU�BP�BL�BF�BC{B9>B-�B'�BBNB
#B��B�_B�VB��B�"B�Bp�BW�B=<B6+B(�B�BBBB �B
�}B
�'B
��B
ňB
�8B
��B
��B
�aB
r�B
fLB
\B
C{B
<PB
-�B
$�B
�B
	B	�B	��B	�B	�B	бB	ǔB	�uB	�B	��B	�:B	�#B	�B	��B	v�B	lWB	a-B	N�B	J�B	;0B	1B	)�B	eB	BB	
#B��B��B�wB�|B�B�wB�kB�B�wB�eB�:B�B��B͹BʌBȀBƎBāB�oB�]B�$B�B� B��B��B��B��B��B�B�FB�B�B�B�B|�B{�By�Bz�Bw�Bv�Bt�Bt�BjeBhXBgRBg8BeFBeFBb4B_B]BX�BT�BQ�BM�BI�BG�BFtBEmBD�BEmBEmBBuB?HB=<B<PB88B6+B4B1�B0�B.�B,�B,�B)�B)�B(�B(�B'�B&�B$�B$�B$�B$�B#�B#�B!�B!�B �B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B"�B#�B%�B&�B%�B%�B#�B%�B%�B$�B&�B)�B(�B,�B/ B1B5%B72B9$B;0B=VB=VB<PB<6B?HB@iBDgBDgBFtBH�BI�BJ�BL�BS�BS�BS�BS�BT�BX�B\�B_Ba-Bc:BeFBfLBgRBh>BiDBjeBm]BmwBncBp�Bu�Bz�B~�B��B��B��B�B�B��B�B�B�B�B�6B�"B�BB�HB�HB�:B�yB�kB�qB�xB�~B��B��B��B��B��B��B� B�B�B�B�DB�<B�oBāBȀB͟B͟BϫBҽB��B��B�B�B�FB�DB�DB�_B�KB�WB�B��B��B��B��B	 �B	�B	�B		B	B	.B	sB	�B	�B	 �B	$�B	'�B	)�B	)�B	+�B	+�B	/ B	2�B	6B	7B	<PB	>BB	?HB	@iB	AoB	C{B	CaB	FtB	I�B	J�B	L�B	M�B	O�B	R�B	S�B	S�B	S�B	U�B	V�B	Y�B	`'B	c B	f2B	g8B	jKB	kkB	lWB	oiB	s�B	x�B	|�B	~�B	��B	��B	��B	�B	�B	�B	�B	�#B	�	B	�6B	�.B	�@B	�SB	�YB	�eB	�qB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�2B	�8B	�$B	�6B	�VB	�]B	�iB	�oB	�{B	�gB	�tB	ȀB	ȚB	ȀB	ɠB	ȚB	ɠB	ˬB	ΥB	��B	ϫB	ϫB	��B	ϫB	��B	��B	бB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�B	�'B	�'B	�B	�B	�B	�,B	�>B	�XB	�DB	�KB	�kB	�WB	�wB	�]B	�cB	�cB	�iB	�B	�vB	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
B
B
B
B
�B
�B
�B
B
	B
	B
	B
	B

	B

	B

#B

	B

	B
B
B
B
B
)B
0B
B
B
B
B
B
6B
6B
"B
"B
"B
"B
"B
(B
NB
NB
4B
TB
@B
[B
FB
FB
FB
MB
mB
mB
mB
sB
YB
yB
_B
_B
_B
_B
eB
eB
eB
eB
B
�B
�B
qB
�B
xB
xB
xB
�B
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
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
+�B
+�B
,�B
+�B
+�B
+�B
,�B
,�B
,�B
-�B
-�B
-�B
-�B
.�B
.�B
/ B
/ B
.�B
0B
/�B
/�B
0�B
1�B
2B
1�B
1�B
2�B
3B
2�B
3B
2�B
2�B
3B
2�B
4B
5B
5B
5B
5%B
5B
6B
6B
6B
6B
6B
7B
72B
72B
7B
7B
7B
88B
88B
9$B
9>B
9$B
9>B
9$B
9>B
9>B
:*B
:*B
:*B
;0B
;0B
<6B
=<B
=<B
=<B
=<B
=VB
=<B
>BB
>]B
>BB
>BB
>]B
?HB
?cB
?cB
?cB
@OB
@iB
@OB
@OB
@iB
@OB
@OB
@iB
@OB
AoB
AoB
AUB
AUB
AUB
AUB
AUB
B[B
B[B
B[B
CaB
C{B
CaB
C{B
DgB
D�B
EmB
EmB
E�B
E�B
FtB
FtB
F�B
FtB
FtB
FtB
FtB
GzB
GzB
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
J�B
K�B
K�B
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
M�B
M�B
N�B
N�B
N�B
O�B
O�B
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
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
ZB
Y�B
Y�B
ZB
Y�B
ZB
ZB
[	B
Z�B
Z�B
[	B
[	B
Z�B
[	B
Z�B
Z�B
[	B
\B
[�B
[�B
[�B
\B
[�B
\B
[�B
]B
\�B
\�B
]B
]B
^B
^B
^B
^B
^B
^B
_B
_B
`B
`'B
`B
`B
`B
`'B
`'B
`'B
`B
a-B
aB
a-B
aB
aB
bB
b4B
bB
c B
c:B
c:B
c B
c B
c B
c:B
d@B
d&B
d&B
d&B
d@B
d@B
eFB
e,B
e,B
e,B
f2B
f2B
fLB
f2B
f2B
g8B
g8B
gRB
g8B
gRB
h>B
h>B
hXB
h>B
h>B
hXB
iDB
iDB
i_B
i_B
i_B
jKB
jKB
jeB
jKB
kQB
kQB
kkB
kQB
kkB
kQB
kQB
kQB
kQB
lWB
lqB
lWB
lq11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.33(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201812190037352018121900373520181219003735201812200024592018122000245920181220002459JA  ARFMdecpA19c                                                                20181214003624  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181213153625  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181213153626  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181213153627  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181213153627  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181213153627  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181213153627  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181213153627  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181213153628  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181213153628                      G�O�G�O�G�O�                JA  ARUP                                                                        20181213155504                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181213154054  CV  JULD            G�O�G�O�Fľ�                JM  ARGQJMQC2.0                                                                 20181213154054  CV  JULD_LOCATION   G�O�G�O�Fľ�                JM  ARGQJMQC2.0                                                                 20181213154054  CV  LATITUDE        G�O�G�O�A�7L                JM  ARCAJMQC2.0                                                                 20181218153735  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181218153735  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181219152459  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021520                      G�O�G�O�G�O�                