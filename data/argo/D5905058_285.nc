CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-12-29T21:39:40Z creation;2020-12-29T21:39:43Z conversion to V3.1;2023-06-29T05:47:36Z update;     
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
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �T   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20201229213940  20230705041504  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0675_285                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�R���m 1   @�R���O�@6=�hr�!�b�p:�~�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bw��B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DUfDU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ Dܼ�D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D�|�D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@ʏ\AG�A%G�AEG�AeG�A���A���A���A���A£�Aң�A��A��BQ�B	Q�BQ�BQ�B!Q�B)Q�B1Q�B9Q�BAQ�BIQ�BQQ�BYQ�BaQ�BiQ�BqQ�Bx�B���B��)B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĨ�BȨ�B̨�BШ�BԨ�Bب�Bܨ�B��B��B��B��B��B���B���B���C nCT{CT{CT{CT{C
T{CT{CT{CT{CT{CT{CT{CT{CT{CT{CT{C T{C"T{C$T{C&T{C(T{C*T{C,T{C.T{C0T{C2T{C4T{C6T{C8T{C:T{C<T{C>T{C@T{CBT{CDT{CFT{CHT{CJT{CLT{CNT{CPT{CRT{CTT{CVT{CXT{CZT{C\T{C^T{C`T{CbT{CdT{CfT{ChT{CjT{ClT{CnT{CpT{CrT{CtT{CvT{CxT{CzT{C|T{C~T{C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D�D�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DU�DU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�DuDu�DvDv�DwDw�DxDx�DyDy�DzDz�D{D{�D|D|�D}D}�D~D~�DD�D�
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
�D�J�D���D�ʏD�
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
�D�J�DǊ�D�ʏD�
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
�D�J�D܊�D��\D�
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
�D�J�D芏D��\D�
�D�J�D�\D��\D�
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
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�1'A�$�A�$�A�&�A�&�A�(�A�1'A�5?A�7LA�33A�/A�1'A�9XA�9XA�7LA�7LA�9XA�?}A�A�A�G�A�M�A���AöFA�|�A�n�A�;dA²-A�hsA��A�ZA��mA��A���A��wA��DA�bNA�-A���A�S�A�G�A��A�33A���A���A���A�ȴA���A��+A�bNA�"�A���A�G�A��A�S�A�bNA��#A�JA�r�A�?}A���A�(�A���A�{A�ZA��jA��A��FA��PA��jA�"�A���A�/A��A���A���A�^5A�hsA�XA�A�=qA���A���A�r�A�(�A�l�A�A���A�%A��7A��A~E�A|�A{�Ay��Aw%Asl�Aq��An�/Al�yAk��AiG�Ag��Ae�hAcAbn�A^{A\�HA\ZAZ��AY33AX��AX5?AW��AW�PAV�!AS��AP�AO�7AL��AK�AHM�AF�9AEG�AC��AB�DA@��A?��A?7LA=�7A;A:�\A: �A9��A8�+A7`BA4ȴA3��A2��A1l�A0ĜA/�7A/%A-��A,��A+�
A*��A*�!A)�FA)?}A)�A(��A(�DA'hsA&�A&bNA%A#G�A"��A!�A =qA33A|�A�A��A�AE�A`BA�#AĜAn�A$�A;dA��A"�A��AM�A��AK�A�AA
��A	�A	?}A��A  A�FA��A
=A^5A��A(�Al�A�AA��A�+A�A �A�A ��@��@��j@�33@��7@�j@�bN@�Z@�hs@�r�@�M�@���@��^@���@��
@��T@@�@��@�5?@�z�@�v�@�7@���@��T@���@�b@�@��@ܴ9@۶F@ڗ�@�^5@�V@�1@�l�@���@��@��@���@��@Ѓ@�|�@�O�@�K�@ʰ!@�`B@���@�z�@�I�@�+@�O�@��@ÍP@�E�@��@�(�@��H@�{@�x�@�7L@�9X@��@���@���@�p�@�V@���@� �@�|�@�dZ@�K�@�o@���@�{@�r�@��
@���@���@�M�@��@���@�V@��@���@��@��-@���@�X@��@�@��+@���@���@�?}@���@�j@��w@��F@�l�@�@���@�M�@��@���@�`B@�V@��`@��j@�bN@���@���@�~�@�5?@�{@�@�x�@�&�@���@��9@��@� �@�Z@� �@��;@�t�@�+@�
=@���@���@�@��@���@�ȴ@���@�V@�-@�J@��#@��h@��h@��@�7L@��j@�z�@�Q�@��m@��H@�n�@�{@��-@���@��@�O�@�&�@���@���@�Ĝ@�Ĝ@��@��@�bN@�b@��F@��P@�\)@�+@���@��y@��y@��@��!@�~�@��@�O�@�&�@��@���@�bN@�A�@�  @��m@�ƨ@���@�dZ@�S�@�C�@�o@�@�ȴ@�M�@��@�{@�@���@���@��7@�7L@�%@��`@��9@�z�@�A�@� �@�b@��;@���@�l�@�;d@��@�
=@���@�~�@�n�@���@���@��@�x�@�p�@�X@�G�@�&�@�%@���@��/@���@��@�z�@�1'@�ƨ@���@�t�@�l�@�\)@�K�@�C�@�+@�o@���@��@���@�=q@��@���@�X@�&�@��@��u@�r�@�9X@�1@���@�ƨ@���@�\)@�;d@��H@��R@��+@�^5@�$�@��T@���@�O�@�&�@���@��j@��@���@��@�Q�@�ƨ@���@��@�S�@�+@��@��@��\@�v�@�M�@�E�@�E�@�@��#@��7@�p�@�?}@��j@��D@�I�@�1@�@�@~�R@~V@}�T@}�@}p�@}V@|�/@|z�@{ƨ@{33@{o@z�@z�\@z-@y��@yhs@yG�@y7L@x��@x�9@xbN@w��@v�@v{@u�h@u�h@u�@up�@uO�@t��@t�D@tj@t9X@s�
@st�@r�@r�H@r��@r��@r~�@q��@qhs@q�@p��@p�`@p�`@p�u@p �@o�;@o��@n��@n��@nv�@m�-@m/@m/@m/@m/@m�@m/@m�@m�@l��@lZ@k�
@k�F@k�@kC�@j�H@j�\@j=q@j�@i��@ix�@iX@iX@iX@iX@iG�@h��@h�9@hr�@g�;@g|�@g;d@f�y@fȴ@f�R@fv�@e�@e�-@e�-@e�@d�@d��@c��@ct�@cS�@cS�@c�@c�@b��@bM�@b=q@aG�@a7L@a&�@a&�@`��@`�`@_�@_\)@_+@_�@^ȴ@^ff@^$�@]�h@]`B@]/@\�j@\�D@\9X@\�@[��@[��@[t�@[S�@Z�\@Yhs@Y%@X��@XĜ@X��@X��@X�9@X�u@XA�@W��@W�@W
=@V�y@Vȴ@V��@Vff@U�h@U`B@U/@T�j@Tz�@TZ@S�
@SdZ@R�H@R�!@R��@Q��@Q&�@Q%@P��@P��@P�`@PĜ@P�u@Pb@O�@O�;@Ol�@O+@N�R@Nff@NV@M�T@MO�@M/@L��@Lj@L(�@K��@K�
@Kƨ@K��@KdZ@K@J�@J�!@J=q@I��@Ix�@IG�@H��@Hr�@HA�@H �@G��@G|�@G;d@G�@F��@F�y@F�R@F��@F�+@F5?@E��@E`B@E?}@D��@D�D@D1@C��@CdZ@CC�@C"�@B�\@BM�@A��@A��@A�@A��@A��@AX@A&�@@�`@@Ĝ@@��@@��@@A�@?�@?�w@?�@?|�@?K�@>�y@>ff@=p�@=`B@=O�@=?}@<��@<�/@<�j@<j@;t�@;S�@;"�@:�@:~�@:=q@:�@:J@:J@:J@9��@9��@9��@9��@9�^@9��@9��@9�7@9x�@9hs@97L@9�@8��@8bN@81'@7�P@6ȴ@6�+@6E�@5�@5�-@5/@5V@4��@4�@4��@4z�@4�@3�
@3�F@3S�@3@2�!@2��@2n�@2-@1�#@1hs@0�`@0��@0r�@0A�@0b@/�w@/|�@/l�@/;d@/+@/+@/+@/�@/
=@.��@.�y@.�@.�@.��@.{@-�T@-�-@-O�@-�@,��@,�/@,��@,�j@,Z@+�m@+�
@+ƨ@+��@+dZ@+S�@+33@*��@*~�@*J@)�^@)��@)��@)x�@)X@)G�@)%@(Q�@'�@'�w@'�@'��@'��@'|�@'\)@'\)@'K�@'�@&��@&��@&ff@&@%�@%`B@%�@$�@$�D@$(�@#ƨ@#�@#"�@#o@"�@"�!@"�\@"n�@"=q@!��@!�#@!�^@!�7@!X@!�@!%@ �9@ �u@ Q�@ b@��@|�@�@
=@�y@��@v�@$�@{@@�T@��@p�@`B@�@��@�@��@z�@9X@�@�
@�F@�F@��@S�@o@��@n�@=q@-@��@�#@��@X@&�@�@�@��@Ĝ@�@bN@1'@�;@�w@�w@�@l�@��@ȴ@v�@{@�T@@��@�@p�@`B@?}@�@V@��@��@�j@��@Z@9X@�@�m@�
@�
@ƨ@�F@��@�@t�@dZ@dZ@S�@C�@@�H@��@��@�!@�!@^5@�@�#@�7@G�@�@Ĝ@�u@A�@b@�@l�@;d@��@ȴ@ff@E�@5?@{@@�@@�-@��@�h@?}@�@��@�D@z�@j@Z@I�@�m@��@dZ@C�@@
�!@
�\@
�@
J@	�@	�@	�#111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�1'A�$�A�$�A�&�A�&�A�(�A�1'A�5?A�7LA�33A�/A�1'A�9XA�9XA�7LA�7LA�9XA�?}A�A�A�G�A�M�A���AöFA�|�A�n�A�;dA²-A�hsA��A�ZA��mA��A���A��wA��DA�bNA�-A���A�S�A�G�A��A�33A���A���A���A�ȴA���A��+A�bNA�"�A���A�G�A��A�S�A�bNA��#A�JA�r�A�?}A���A�(�A���A�{A�ZA��jA��A��FA��PA��jA�"�A���A�/A��A���A���A�^5A�hsA�XA�A�=qA���A���A�r�A�(�A�l�A�A���A�%A��7A��A~E�A|�A{�Ay��Aw%Asl�Aq��An�/Al�yAk��AiG�Ag��Ae�hAcAbn�A^{A\�HA\ZAZ��AY33AX��AX5?AW��AW�PAV�!AS��AP�AO�7AL��AK�AHM�AF�9AEG�AC��AB�DA@��A?��A?7LA=�7A;A:�\A: �A9��A8�+A7`BA4ȴA3��A2��A1l�A0ĜA/�7A/%A-��A,��A+�
A*��A*�!A)�FA)?}A)�A(��A(�DA'hsA&�A&bNA%A#G�A"��A!�A =qA33A|�A�A��A�AE�A`BA�#AĜAn�A$�A;dA��A"�A��AM�A��AK�A�AA
��A	�A	?}A��A  A�FA��A
=A^5A��A(�Al�A�AA��A�+A�A �A�A ��@��@��j@�33@��7@�j@�bN@�Z@�hs@�r�@�M�@���@��^@���@��
@��T@@�@��@�5?@�z�@�v�@�7@���@��T@���@�b@�@��@ܴ9@۶F@ڗ�@�^5@�V@�1@�l�@���@��@��@���@��@Ѓ@�|�@�O�@�K�@ʰ!@�`B@���@�z�@�I�@�+@�O�@��@ÍP@�E�@��@�(�@��H@�{@�x�@�7L@�9X@��@���@���@�p�@�V@���@� �@�|�@�dZ@�K�@�o@���@�{@�r�@��
@���@���@�M�@��@���@�V@��@���@��@��-@���@�X@��@�@��+@���@���@�?}@���@�j@��w@��F@�l�@�@���@�M�@��@���@�`B@�V@��`@��j@�bN@���@���@�~�@�5?@�{@�@�x�@�&�@���@��9@��@� �@�Z@� �@��;@�t�@�+@�
=@���@���@�@��@���@�ȴ@���@�V@�-@�J@��#@��h@��h@��@�7L@��j@�z�@�Q�@��m@��H@�n�@�{@��-@���@��@�O�@�&�@���@���@�Ĝ@�Ĝ@��@��@�bN@�b@��F@��P@�\)@�+@���@��y@��y@��@��!@�~�@��@�O�@�&�@��@���@�bN@�A�@�  @��m@�ƨ@���@�dZ@�S�@�C�@�o@�@�ȴ@�M�@��@�{@�@���@���@��7@�7L@�%@��`@��9@�z�@�A�@� �@�b@��;@���@�l�@�;d@��@�
=@���@�~�@�n�@���@���@��@�x�@�p�@�X@�G�@�&�@�%@���@��/@���@��@�z�@�1'@�ƨ@���@�t�@�l�@�\)@�K�@�C�@�+@�o@���@��@���@�=q@��@���@�X@�&�@��@��u@�r�@�9X@�1@���@�ƨ@���@�\)@�;d@��H@��R@��+@�^5@�$�@��T@���@�O�@�&�@���@��j@��@���@��@�Q�@�ƨ@���@��@�S�@�+@��@��@��\@�v�@�M�@�E�@�E�@�@��#@��7@�p�@�?}@��j@��D@�I�@�1@�@�@~�R@~V@}�T@}�@}p�@}V@|�/@|z�@{ƨ@{33@{o@z�@z�\@z-@y��@yhs@yG�@y7L@x��@x�9@xbN@w��@v�@v{@u�h@u�h@u�@up�@uO�@t��@t�D@tj@t9X@s�
@st�@r�@r�H@r��@r��@r~�@q��@qhs@q�@p��@p�`@p�`@p�u@p �@o�;@o��@n��@n��@nv�@m�-@m/@m/@m/@m/@m�@m/@m�@m�@l��@lZ@k�
@k�F@k�@kC�@j�H@j�\@j=q@j�@i��@ix�@iX@iX@iX@iX@iG�@h��@h�9@hr�@g�;@g|�@g;d@f�y@fȴ@f�R@fv�@e�@e�-@e�-@e�@d�@d��@c��@ct�@cS�@cS�@c�@c�@b��@bM�@b=q@aG�@a7L@a&�@a&�@`��@`�`@_�@_\)@_+@_�@^ȴ@^ff@^$�@]�h@]`B@]/@\�j@\�D@\9X@\�@[��@[��@[t�@[S�@Z�\@Yhs@Y%@X��@XĜ@X��@X��@X�9@X�u@XA�@W��@W�@W
=@V�y@Vȴ@V��@Vff@U�h@U`B@U/@T�j@Tz�@TZ@S�
@SdZ@R�H@R�!@R��@Q��@Q&�@Q%@P��@P��@P�`@PĜ@P�u@Pb@O�@O�;@Ol�@O+@N�R@Nff@NV@M�T@MO�@M/@L��@Lj@L(�@K��@K�
@Kƨ@K��@KdZ@K@J�@J�!@J=q@I��@Ix�@IG�@H��@Hr�@HA�@H �@G��@G|�@G;d@G�@F��@F�y@F�R@F��@F�+@F5?@E��@E`B@E?}@D��@D�D@D1@C��@CdZ@CC�@C"�@B�\@BM�@A��@A��@A�@A��@A��@AX@A&�@@�`@@Ĝ@@��@@��@@A�@?�@?�w@?�@?|�@?K�@>�y@>ff@=p�@=`B@=O�@=?}@<��@<�/@<�j@<j@;t�@;S�@;"�@:�@:~�@:=q@:�@:J@:J@:J@9��@9��@9��@9��@9�^@9��@9��@9�7@9x�@9hs@97L@9�@8��@8bN@81'@7�P@6ȴ@6�+@6E�@5�@5�-@5/@5V@4��@4�@4��@4z�@4�@3�
@3�F@3S�@3@2�!@2��@2n�@2-@1�#@1hs@0�`@0��@0r�@0A�@0b@/�w@/|�@/l�@/;d@/+@/+@/+@/�@/
=@.��@.�y@.�@.�@.��@.{@-�T@-�-@-O�@-�@,��@,�/@,��@,�j@,Z@+�m@+�
@+ƨ@+��@+dZ@+S�@+33@*��@*~�@*J@)�^@)��@)��@)x�@)X@)G�@)%@(Q�@'�@'�w@'�@'��@'��@'|�@'\)@'\)@'K�@'�@&��@&��@&ff@&@%�@%`B@%�@$�@$�D@$(�@#ƨ@#�@#"�@#o@"�@"�!@"�\@"n�@"=q@!��@!�#@!�^@!�7@!X@!�@!%@ �9@ �u@ Q�@ b@��@|�@�@
=@�y@��@v�@$�@{@@�T@��@p�@`B@�@��@�@��@z�@9X@�@�
@�F@�F@��@S�@o@��@n�@=q@-@��@�#@��@X@&�@�@�@��@Ĝ@�@bN@1'@�;@�w@�w@�@l�@��@ȴ@v�@{@�T@@��@�@p�@`B@?}@�@V@��@��@�j@��@Z@9X@�@�m@�
@�
@ƨ@�F@��@�@t�@dZ@dZ@S�@C�@@�H@��@��@�!@�!@^5@�@�#@�7@G�@�@Ĝ@�u@A�@b@�@l�@;d@��@ȴ@ff@E�@5?@{@@�@@�-@��@�h@?}@�@��@�D@z�@j@Z@I�@�m@��@dZ@C�@@
�!@
�\@
�@
J@	�@	�@	�#111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BBDB?}BbNBt�Bu�Bv�Bw�Bu�Bq�BjBgmBVBO�BM�BM�BM�BL�BJ�BJ�BJ�BQ�BS�BYBW
BXBZBZBYBZBYBXBW
BT�BR�BO�BK�BH�BB�B9XB33B+B%�B!�B�B�B\B�B+B�sB��B��B��B�VB�Bt�BT�B=qB,B�B
��B
�NB
�B
��B
ƨB
�^B
�B
��B
� B
`BB
F�B
6FB
,B
�B
VB	��B	�TB	��B	ÖB	�'B	��B	��B	�PB	~�B	o�B	gmB	O�B	B�B	=qB	7LB	,B	'�B	$�B	!�B	�B	�B	JB��B�B�TB�#B��BÖB�jB�FB�!B��B��B��B��B�uB�bB�\B�PB�DB�+B�B}�Bz�Bx�Bw�Bs�Bq�Bn�Bk�BiyBe`BdZBdZBbNBaHB`BB`BB]/B[#BZBXBT�BR�BP�BM�BL�BK�BI�BL�BK�BJ�BI�BJ�BG�BI�BC�BA�B@�BB�BA�BB�BC�BD�BD�BH�BG�BF�BF�BH�BI�BH�BG�BE�BB�B@�B:^B33B8RB>wBB�B8RB33B:^B>wB@�B;dB0!B+B(�B(�B,B2-B:^BC�BA�BD�BF�BG�BG�BI�BH�BK�BL�BL�BN�BS�BQ�BP�BR�BT�BVBW
BT�BVBVBW
BXBYBYBYBXBXBYBZB[#B\)B_;BbNBe`BffBgmBn�Bp�Bq�Bs�Bv�Bw�Bw�Bz�B~�B�B�B�+B�=B�=B�hB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�3B�9B�FB�XB�qB��BÖBÖBÖBÖBǮB��B��B��B��B��B��B�B�5B�;B�ZB�B�B�B��B��B��B��B��B��B��B��B	  B	  B	  B	  B	B	B	%B		7B	
=B	DB	PB	�B	�B	�B	�B	#�B	#�B	#�B	$�B	$�B	%�B	)�B	)�B	+B	,B	-B	.B	0!B	8RB	=qB	>wB	A�B	C�B	F�B	H�B	M�B	S�B	T�B	W
B	ZB	ZB	\)B	_;B	bNB	dZB	gmB	hsB	iyB	jB	k�B	k�B	p�B	s�B	t�B	u�B	u�B	w�B	y�B	{�B	|�B	}�B	~�B	�B	�B	�B	�B	�+B	�7B	�=B	�JB	�PB	�PB	�VB	�\B	�hB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�3B	�3B	�LB	�^B	�dB	�dB	�dB	�jB	�qB	�}B	��B	��B	B	ÖB	ÖB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�/B	�/B	�;B	�;B	�;B	�BB	�HB	�HB	�NB	�TB	�ZB	�ZB	�`B	�fB	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
%B
+B
+B
+B
1B
1B
	7B
DB
DB
DB
DB
DB
JB
JB
JB
PB
PB
PB
PB
VB
VB
VB
\B
\B
bB
hB
hB
hB
hB
hB
oB
oB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
#�B
#�B
#�B
$�B
$�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
,B
,B
-B
,B
-B
.B
/B
/B
/B
/B
/B
/B
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
33B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
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
:^B
;dB
;dB
<jB
<jB
<jB
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
>wB
>wB
>wB
?}B
?}B
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
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
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
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
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
S�B
S�B
T�B
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
XB
YB
YB
YB
YB
YB
YB
YB
YB
ZB
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
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
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
gmB
hsB
hsB
hsB
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
l�B
l�B
m�B
m�B
m�B
m�B
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
q�B
r�B
r�B
r�B
r�B
r�B
s�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B3B0B?�Bb�Bt�Bv`Bw�Bx�Bw�BwLBs�Bp�BX+BPbBN<BN<BN�BN�BN<BO\BO�BV9BXyB[=BXBX_BZkBZ7BYeBZ�BY�BY1BW�BV�BT�BQ4BMjBJXBD�B:�B49B+�B'mB#TBB�B B�B)B�qB��B�mB�KB��B��By	BW?B@4B/iBWB  B
��B
�$B
̳B
�fB
�jB
�GB
��B
�B
cnB
H�B
8B
./B
�B
�B
�B	�B	�$B	��B	��B	�_B	��B	�B	�;B	rB	kkB	QhB	C�B	?HB	8�B	,�B	(XB	%zB	"hB	!�B	B	\B��B��B��B�B͟B�SB�BB��B�-B�0B��B��B�!B�B�B�HB��B�B��B��B.B|PBy�By$Bt�Bs3Bo�Bl�BjKBe�Be`Bd�BbhBa�B`�Ba|B]�B\CB[�BZBVBT�BR BO\BN�BL�BJ�BN�BL�BL0BK�BL�BJ�BLBD�BB�BB'BCBA�BCGBDBESBFBI�BH�BG�BGEBI�BJ	BIBHfBF�BC�BB'B:�B3B8B>�BC{B8�B3�B;0B?}BA�B=B1B,B)yB(�B+�B1�B;BD�BA�BD�BGEBH�BIBK�BJ	BL�BMjBM�BO�BT�BS&BQ�BS�BU�BV�BW�BU�BV�BV�BWsBX�BY�BY�BY�BX�BX�BY�BZ�B\)B]B`�BcTBe�BgBg�Bn�Bp�Br|Bt�Bw�Bx8Bx�B{�BcB��B��B�zB�XB��B��B��B�,B�$B��B��B��B�B��B��B��B�B�:B��B�*B�B��B�UB�MB��B��B�B�(B��BÖBÖB��B�3BȀB��B�.B�B�@B�,B�MB�eB�B�pB�B�B��B��B��B�B��B��B��B�0B�jB�cB	 4B	 B	  B	 B	AB	MB	?B		7B	
XB	DB	6B	�B	�B	�B	�B	#�B	#�B	#�B	$�B	$�B	%�B	)�B	)�B	+B	,B	-B	.B	0;B	88B	=VB	>�B	A�B	C�B	F�B	IB	NVB	TB	UB	W$B	ZB	ZB	\CB	_;B	bNB	dZB	gRB	hXB	iyB	jB	k�B	k�B	p�B	s�B	t�B	u�B	u�B	w�B	y�B	{�B	|�B	~(B	HB	�UB	�B	�B	�3B	�+B	�7B	�XB	�0B	�6B	�PB	�VB	�BB	�NB	�oB	�{B	�{B	��B	��B	��B	�yB	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�OB	�-B	�B	�MB	�LB	�^B	�JB	�JB	�JB	�jB	�VB	�cB	�iB	�oB	�uB	�{B	ðB	ĶB	��B	ɠB	̳B	͹B	��B	οB	��B	��B	��B	��B	��B	��B	�B	��B	�B	�
B	�B	�B	�#B	�B	�/B	�!B	�!B	�;B	�BB	�HB	�bB	�NB	�TB	�ZB	�ZB	�`B	�fB	�fB	�mB	�B	�sB	�yB	�eB	�eB	�B	�B	�B	�qB	�qB	�wB	�B	�}B	�B	�B	��B	��B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B
  B
 B
B
 �B
 �B
�B
�B
B
-B
MB
9B
%B
�B
%B
B
B
?B
B
B
B
KB
1B
	7B
B
DB
DB
)B
DB
0B
JB
0B
B
6B
6B
jB
<B
<B
VB
BB
\B
�B
hB
NB
4B
NB
4B
:B
TB
TB
TB
uB
�B
gB
gB
mB
�B
�B
sB
sB
�B
B
�B
B
eB
B
B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
#�B
#�B
$�B
$�B
%�B
&�B
&�B
'B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
)*B
*0B
)�B
)�B
*�B
*�B
+�B
+�B
,�B
,B
-)B
.B
/ B
/ B
/ B
/ B
/ B
/5B
1'B
1'B
1AB
2B
2B
2-B
2-B
33B
3B
3B
3MB
4TB
5%B
5B
5%B
5%B
5?B
5%B
5?B
6FB
6FB
6FB
6+B
7fB
72B
7LB
7LB
8lB
88B
8lB
9XB
9>B
9XB
9XB
9>B
9>B
9XB
:DB
:DB
:^B
:xB
;dB
;dB
<jB
<�B
<PB
=VB
=VB
=VB
=qB
=VB
>wB
>wB
>]B
>]B
>]B
>]B
>wB
>�B
?cB
?}B
?}B
@�B
@�B
@�B
AoB
AoB
AoB
A�B
AoB
B�B
B[B
BuB
BuB
B�B
B�B
BuB
C{B
C�B
C{B
C{B
C{B
C{B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
F�B
FtB
F�B
F�B
F�B
F�B
F�B
F�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
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
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
Q B
Q B
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
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
U�B
U�B
VB
VB
VB
U�B
VB
VB
W
B
V�B
W�B
W�B
W�B
XB
W�B
XB
XB
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
X�B
ZB
ZB
ZB
ZB
Z7B
[	B
[#B
[	B
[#B
[=B
\)B
\)B
\)B
]B
]B
]B
]B
]/B
]/B
]/B
]/B
]B
^B
^B
^B
^5B
^B
^B
^B
_;B
_;B
_!B
_;B
`'B
`'B
`BB
`'B
`BB
`B
`'B
`'B
aHB
a-B
a-B
aHB
a-B
aHB
a-B
bNB
b4B
bNB
b4B
bNB
c B
cTB
c:B
cTB
cTB
c:B
d@B
d@B
d@B
d@B
d@B
d@B
d@B
d&B
d@B
dZB
dZB
dZB
dZB
e`B
e`B
eFB
f2B
fLB
fLB
f�B
gRB
gmB
gRB
hXB
hXB
hXB
hsB
h>B
hXB
hXB
hXB
h>B
hsB
hsB
i_B
iyB
i_B
i_B
i_B
iyB
i_B
jKB
jKB
jeB
jKB
jeB
jKB
jKB
jeB
jKB
jeB
jB
kQB
kkB
kQB
kQB
kkB
k�B
kkB
kkB
k�B
lqB
l�B
lqB
lqB
l�B
m�B
mwB
mwB
mwB
n�B
n�B
n�B
o�B
oiB
o�B
oiB
o�B
o�B
o�B
oiB
o�B
o�B
p�B
p�B
p�B
p�B
poB
poB
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
r|B
r�B
s�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.33(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202101040037262021010400372620210104003726202306231726502023062317265020230623172650202101050035302021010500353020210105003530  JA  ARFMdecpA19c                                                                20201230063928  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20201229213940  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20201229213942  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20201229213942  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20201229213942  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20201229213942  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20201229213942  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20201229213942  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20201229213943  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20201229213943                      G�O�G�O�G�O�                JA  ARUP                                                                        20201229215203                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20201230153637  CV  JULD            G�O�G�O�Fʕm                JM  ARGQJMQC2.0                                                                 20201230153637  CV  JULD_LOCATION   G�O�G�O�Fʕ�                JM  ARGQJMQC2.0                                                                 20201230153637  CV  LATITUDE        G�O�G�O�A��                JM  ARCAJMQC2.0                                                                 20210103153726  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20210103153726  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2019V1                                                       20210104153530  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623082650  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705041504                      G�O�G�O�G�O�                