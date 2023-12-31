CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-11-29T15:37:16Z creation;2019-11-29T15:37:21Z conversion to V3.1;2022-11-21T05:27:58Z update;     
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
_FillValue                 �  ]\   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  aL   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �8   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ߈   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �X   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �h   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �l   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �|   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20191129153716  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_193                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @����� 1   @��& @;�ߤ?���dn=p��
1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @,��@�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��fD�3D�&f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @!�@u�@��\@�\)AG�A=G�A]G�A}G�A���A���A���A���AΣ�Aޣ�A��A���BQ�BQ�BQ�BQ�B'Q�B/Q�B7Q�B?Q�BGQ�BOQ�BWQ�B_Q�BgQ�BoQ�BwQ�BQ�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bè�BǨ�B˨�BϨ�BӨ�Bר�Bۨ�Bߨ�B��B��B��B��B��B���B���B���C�{C�{C�{C�{C	�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C!�{C#�{C%�{C'�{C)�{C+�{C-�{C/�{C1�{C3�{C5�{C7�{C9�{C;�{C=�{C?�{CA�{CC�{CE�{CG�{CI�{CK�{CM�{CO�{CQ�{CS�{CU�{CW�{CY�{C[�{C]�{C_�{Ca�{Cc�{Ce�{Cg�{Ci�{Ck�{Cm�{Co�{Cq�{Cs�{Cu�{Cw�{Cy�{C{�{C}�{C�{C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��
C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=D uD �DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D	uD	�D
uD
�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D uD �D!uD!�D"uD"�D#uD#�D$uD$�D%uD%�D&uD&�D'uD'�D(uD(�D)uD)�D*uD*�D+uD+�D,uD,�D-uD-�D.uD.�D/uD/�D0uD0�D1uD1�D2uD2�D3uD3�D4uD4�D5uD5�D6uD6�D7uD7�D8uD8�D9uD9�D:uD:�D;uD;�D<uD<�D=uD=�D>uD>�D?uD?�D@uD@�DAuDA�DBuDB�DCuDC�DDuDD�DEuDE�DFuDF�DGuDG�DHuDH�DIuDI�DJuDJ�DKuDK�DLuDL�DMuDM�DNuDN�DOuDO�DPuDP�DQuDQ�DRuDR�DSuDS�DTuDT�DUuDU�DVuDV�DWuDW�DXuDX�DYuDY�DZuDZ�D[uD[�D\uD\�D]uD]�D^uD^�D_uD_�D`uD`�DauDa�DbuDb�DcuDc�DduDd�DeuDe�DfuDf�DguDg�DhuDh�DiuDi�DjuDj�DkuDk�DluDl�DmuDm�DnuDn�DouDo�DpuDp�DquDq�DruDr�DsuDs�DtuDt�DuuDu�DvuDv�DwuDw�DxuDx�DyuDy�DzuDz�D{uD{�D|uD|�D}uD}�D~uD~�DuD�D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�DຏD���D�:�D�z�DẏD���D�:�D�z�D⺏D���D�:�D�z�D㺏D���D�:�D�z�D亏D���D�:�D�z�D序D���D�:�D�z�D溏D���D�:�D�z�D纏D���D�:�D�w\D躏D���D�:�D�z�D麏D���D�:�D�z�D꺏D���D�:�D�z�D뺏D���D�:�D�z�D캏D���D�:�D�z�D���D���D�:�D�z�DD���D�:�D�z�DﺏD���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D� �111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A�%A�A�1A�&�A�$�A�&�A�(�A��A���A���A�A�ĜA��^A��RA��FA��!A��9A��^A��FA��!A���A���A���A���A�z�A�XA��A���A��A��PA�^5A��hA�`BA��;A���A�E�A�\)A��!A��hA��#A�n�A���A��A�Q�A��hA�33A�bA���A�|�A��7A�  A��#A�JA��/A��hA�&�A�ĜA��A���A�t�A�33A��-A���A�^5A�A�A���A���A�t�A��A��A��9A�=qA��A��A��-A�9XA��A�S�A�Q�A���A�`BA�G�A�A}��A|Az=qAw�
As��Aql�ApȴAp�ApE�Ap1Ao��An��Ak�AjbNAi��AhE�Agx�AfĜAe|�Ad�AcdZAcoAb�`Ab��Aa�A_+A^�HA^�/A^��A]�7A[�7AY��AXr�AW�mAW�AW�AW;dAW&�AV�!AVbAUp�AT�AT��AS�mAR�`AQ�7AOdZANAM�;AMAMdZAK�PAK"�AJn�AJE�AI"�AH~�AH1AG�AG;dAF��AF�AF��ADĜAC��AC�^ACXAB��AAƨA@�A@�!A@�A?��A?7LA>�A>(�A=�hA;l�A9S�A7p�A6�A6~�A6E�A6�A5�;A5|�A49XA2(�A0��A0-A/�
A/��A/K�A.��A. �A,ȴA, �A+�A*�+A*I�A*�A)��A)oA(��A'�mA&�A$��A$=qA#t�A"��A"�RA"�\A"A!t�A!�A �jA Q�A�;A?}A^5A\)A�yAffA��A�9AM�A�mAS�A"�AVA�FAVA�9A^5A  A��AO�A�9AO�A�RA�A�PA��A�A  AG�A~�A�FAG�A
�RA	`BA��AQ�A�hA�A�A��A�A��AA �A J@���@��P@��y@���@���@�r�@��@���@���@��;@�5?@�K�@�hs@���@�1'@�l�@�R@�^@�7L@���@�@��@�~�@�-@��@�/@�Z@߮@�E�@��@�~�@ׅ@���@��/@�I�@�K�@�E�@�7L@�bN@�Q�@�I�@�1@���@�7L@̛�@˾w@�@�^5@��T@�`B@���@�A�@Ǿw@�K�@ƸR@Ƈ+@�7L@ċD@�bN@�@�ff@�@��^@��h@���@�Q�@�ȴ@�&�@�~�@�7L@�Ĝ@��D@�bN@��
@���@�@���@�?}@���@�(�@���@���@���@�ff@�{@���@�1@��@�
=@�M�@��h@��@�(�@��\@���@���@�r�@��@��y@��+@�v�@�n�@�M�@�-@�X@��@��@�t�@���@��R@�V@�@��@�O�@�V@���@���@�C�@���@�E�@�{@���@��/@�bN@��w@�t�@���@�v�@�@�x�@��u@�1@�ƨ@��y@�ff@�E�@���@��u@��@��@�K�@��@���@�n�@��@��7@�G�@�/@��@���@�Ĝ@�bN@���@�l�@�K�@��y@�~�@�5?@�J@��^@��@��u@�Z@�ƨ@�t�@���@��@��#@��-@�`B@���@�r�@�b@��m@��F@�ȴ@�J@��@���@���@��@�?}@���@�z�@�|�@�33@�+@�o@�ȴ@��\@�M�@�{@�@��@��-@�`B@�&�@�V@���@��D@�A�@|�@K�@~�R@~ff@~{@}@}�h@|�@{�m@{C�@{@zJ@y�#@yx�@x��@x��@x  @w�P@v�y@v5?@u�T@u��@u�@u?}@t�@t��@tj@t�@s��@so@r��@r=q@q��@q��@q��@q�7@qx�@q&�@q%@pĜ@p�9@o�;@o�P@oK�@nv�@n$�@m�h@m?}@m?}@mV@m`B@m@m�T@mp�@m/@mV@l��@l��@l��@l1@kdZ@ko@k@k@j�@j��@j�\@j�\@j~�@j^5@jM�@j-@i�#@ihs@h�@g�;@gl�@f�y@f{@e�@dZ@d1@c��@c�F@cC�@c@b�H@b�!@b�!@b=q@a��@`Ĝ@`r�@_��@^5?@^$�@^@]��@]`B@]�@\�j@\Z@\�@[��@[C�@[o@Z��@Z~�@Z^5@Z-@Y�@Y��@Y�@X�9@X�@X1'@W��@V�y@VE�@V@U��@U��@Up�@UO�@U�@T�/@T�@Tz�@S��@S��@S�@St�@R�@R�!@R�\@R^5@R=q@RJ@Q�#@Q�^@Q��@PĜ@Pb@O|�@N��@Nȴ@N��@Nff@M�T@M�@M?}@L�j@Lz�@LI�@L1@K�@KC�@K@J�!@J~�@JM�@I�#@Ix�@H��@HĜ@HĜ@H��@H��@HQ�@G�@G�P@G;d@F�y@Fȴ@F�R@F�+@Fff@FV@FV@FE�@FE�@FE�@F5?@F$�@F{@E�T@E��@E@E�h@E�@D��@DZ@D(�@C�
@C�@C@B�!@B=q@B�@A��@A��@Ax�@A%@A%@@�`@@Q�@@1'@@  @?�P@?\)@?;d@>�@>�+@>v�@>E�@=��@=�h@=�@=�@=?}@<�@<�D@<1@;��@;��@;t�@;C�@:�@:��@:n�@:J@9�#@9��@9��@9G�@97L@9&�@9�@9%@8�`@8�`@8�9@8bN@81'@8b@7�@7�;@7��@7|�@7|�@7+@6�@6ff@65?@5�@5@5�-@5`B@5?}@5�@4��@4�j@49X@41@3��@3"�@2�@2��@2n�@2M�@1��@1��@1��@1x�@1x�@17L@0�`@0��@0�u@0�@0Q�@01'@0b@/��@/l�@/+@/
=@.��@.�y@.�y@.��@.5?@-p�@-�@,��@,�@,�/@,��@,�@,�D@,9X@+�
@+S�@+"�@+o@+@*��@*~�@)�@)��@)x�@)7L@(�9@(bN@( �@(  @'�@'�w@'|�@'l�@'K�@';d@'+@'
=@&ȴ@&��@&ff@&V@&E�@&5?@&@%�T@%@%�h@%�h@%�h@%p�@%`B@$�@$z�@#ƨ@#�F@#�F@#dZ@#o@"�!@"~�@"n�@"=q@"�@"J@!�#@!�^@!X@!%@ ��@ Ĝ@ �9@ �u@ Q�@   @�;@�w@��@l�@;d@�@��@v�@V@E�@@`B@�@V@��@�/@�j@j@9X@��@�F@dZ@o@�H@��@n�@M�@J@��@G�@��@r�@A�@A�@ �@  @�@�;@|�@\)@K�@;d@+@��@�R@�+@5?@�@@�-@�@/@�/@�j@�D@9X@�@��@�m@�
@ƨ@�F@��@��@�@dZ@"�@�!@~�@~�@~�@~�@^5@^5@M�@J@�#@�^@��@x�@7L@Ĝ@�@ �@b@�;@�P@|�@l�@l�@K�@;d@
=@��@ȴ@��@�+@ff@$�@�h@`B@/@�@�@�@�@�D@j@1@�F@��@t�@S�@o@o@
�H@
�!@
n�@
^5@
=q@	�#@	��@	�7@	hs@	X@	X@	7L@	&�@	&�@	%@��@�@1'@b@��@��@\)@+@�@�@��@�y@�R@�+@ff@5?@$�@5?@$�@@@�@�-@��@�h@p�@?}@�@�@��@(�@�m@ƨ@�F@��@��@�@33@@�@�H@�H@��@��@��@�!@�\@~�@M�@-@J@�@��@�^@�7@X@�@ ��@ �`@ Ĝ@ �9@ ��@ �@ �@ �@ bN@ A�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A�%A�A�1A�&�A�$�A�&�A�(�A��A���A���A�A�ĜA��^A��RA��FA��!A��9A��^A��FA��!A���A���A���A���A�z�A�XA��A���A��A��PA�^5A��hA�`BA��;A���A�E�A�\)A��!A��hA��#A�n�A���A��A�Q�A��hA�33A�bA���A�|�A��7A�  A��#A�JA��/A��hA�&�A�ĜA��A���A�t�A�33A��-A���A�^5A�A�A���A���A�t�A��A��A��9A�=qA��A��A��-A�9XA��A�S�A�Q�A���A�`BA�G�A�A}��A|Az=qAw�
As��Aql�ApȴAp�ApE�Ap1Ao��An��Ak�AjbNAi��AhE�Agx�AfĜAe|�Ad�AcdZAcoAb�`Ab��Aa�A_+A^�HA^�/A^��A]�7A[�7AY��AXr�AW�mAW�AW�AW;dAW&�AV�!AVbAUp�AT�AT��AS�mAR�`AQ�7AOdZANAM�;AMAMdZAK�PAK"�AJn�AJE�AI"�AH~�AH1AG�AG;dAF��AF�AF��ADĜAC��AC�^ACXAB��AAƨA@�A@�!A@�A?��A?7LA>�A>(�A=�hA;l�A9S�A7p�A6�A6~�A6E�A6�A5�;A5|�A49XA2(�A0��A0-A/�
A/��A/K�A.��A. �A,ȴA, �A+�A*�+A*I�A*�A)��A)oA(��A'�mA&�A$��A$=qA#t�A"��A"�RA"�\A"A!t�A!�A �jA Q�A�;A?}A^5A\)A�yAffA��A�9AM�A�mAS�A"�AVA�FAVA�9A^5A  A��AO�A�9AO�A�RA�A�PA��A�A  AG�A~�A�FAG�A
�RA	`BA��AQ�A�hA�A�A��A�A��AA �A J@���@��P@��y@���@���@�r�@��@���@���@��;@�5?@�K�@�hs@���@�1'@�l�@�R@�^@�7L@���@�@��@�~�@�-@��@�/@�Z@߮@�E�@��@�~�@ׅ@���@��/@�I�@�K�@�E�@�7L@�bN@�Q�@�I�@�1@���@�7L@̛�@˾w@�@�^5@��T@�`B@���@�A�@Ǿw@�K�@ƸR@Ƈ+@�7L@ċD@�bN@�@�ff@�@��^@��h@���@�Q�@�ȴ@�&�@�~�@�7L@�Ĝ@��D@�bN@��
@���@�@���@�?}@���@�(�@���@���@���@�ff@�{@���@�1@��@�
=@�M�@��h@��@�(�@��\@���@���@�r�@��@��y@��+@�v�@�n�@�M�@�-@�X@��@��@�t�@���@��R@�V@�@��@�O�@�V@���@���@�C�@���@�E�@�{@���@��/@�bN@��w@�t�@���@�v�@�@�x�@��u@�1@�ƨ@��y@�ff@�E�@���@��u@��@��@�K�@��@���@�n�@��@��7@�G�@�/@��@���@�Ĝ@�bN@���@�l�@�K�@��y@�~�@�5?@�J@��^@��@��u@�Z@�ƨ@�t�@���@��@��#@��-@�`B@���@�r�@�b@��m@��F@�ȴ@�J@��@���@���@��@�?}@���@�z�@�|�@�33@�+@�o@�ȴ@��\@�M�@�{@�@��@��-@�`B@�&�@�V@���@��D@�A�@|�@K�@~�R@~ff@~{@}@}�h@|�@{�m@{C�@{@zJ@y�#@yx�@x��@x��@x  @w�P@v�y@v5?@u�T@u��@u�@u?}@t�@t��@tj@t�@s��@so@r��@r=q@q��@q��@q��@q�7@qx�@q&�@q%@pĜ@p�9@o�;@o�P@oK�@nv�@n$�@m�h@m?}@m?}@mV@m`B@m@m�T@mp�@m/@mV@l��@l��@l��@l1@kdZ@ko@k@k@j�@j��@j�\@j�\@j~�@j^5@jM�@j-@i�#@ihs@h�@g�;@gl�@f�y@f{@e�@dZ@d1@c��@c�F@cC�@c@b�H@b�!@b�!@b=q@a��@`Ĝ@`r�@_��@^5?@^$�@^@]��@]`B@]�@\�j@\Z@\�@[��@[C�@[o@Z��@Z~�@Z^5@Z-@Y�@Y��@Y�@X�9@X�@X1'@W��@V�y@VE�@V@U��@U��@Up�@UO�@U�@T�/@T�@Tz�@S��@S��@S�@St�@R�@R�!@R�\@R^5@R=q@RJ@Q�#@Q�^@Q��@PĜ@Pb@O|�@N��@Nȴ@N��@Nff@M�T@M�@M?}@L�j@Lz�@LI�@L1@K�@KC�@K@J�!@J~�@JM�@I�#@Ix�@H��@HĜ@HĜ@H��@H��@HQ�@G�@G�P@G;d@F�y@Fȴ@F�R@F�+@Fff@FV@FV@FE�@FE�@FE�@F5?@F$�@F{@E�T@E��@E@E�h@E�@D��@DZ@D(�@C�
@C�@C@B�!@B=q@B�@A��@A��@Ax�@A%@A%@@�`@@Q�@@1'@@  @?�P@?\)@?;d@>�@>�+@>v�@>E�@=��@=�h@=�@=�@=?}@<�@<�D@<1@;��@;��@;t�@;C�@:�@:��@:n�@:J@9�#@9��@9��@9G�@97L@9&�@9�@9%@8�`@8�`@8�9@8bN@81'@8b@7�@7�;@7��@7|�@7|�@7+@6�@6ff@65?@5�@5@5�-@5`B@5?}@5�@4��@4�j@49X@41@3��@3"�@2�@2��@2n�@2M�@1��@1��@1��@1x�@1x�@17L@0�`@0��@0�u@0�@0Q�@01'@0b@/��@/l�@/+@/
=@.��@.�y@.�y@.��@.5?@-p�@-�@,��@,�@,�/@,��@,�@,�D@,9X@+�
@+S�@+"�@+o@+@*��@*~�@)�@)��@)x�@)7L@(�9@(bN@( �@(  @'�@'�w@'|�@'l�@'K�@';d@'+@'
=@&ȴ@&��@&ff@&V@&E�@&5?@&@%�T@%@%�h@%�h@%�h@%p�@%`B@$�@$z�@#ƨ@#�F@#�F@#dZ@#o@"�!@"~�@"n�@"=q@"�@"J@!�#@!�^@!X@!%@ ��@ Ĝ@ �9@ �u@ Q�@   @�;@�w@��@l�@;d@�@��@v�@V@E�@@`B@�@V@��@�/@�j@j@9X@��@�F@dZ@o@�H@��@n�@M�@J@��@G�@��@r�@A�@A�@ �@  @�@�;@|�@\)@K�@;d@+@��@�R@�+@5?@�@@�-@�@/@�/@�j@�D@9X@�@��@�m@�
@ƨ@�F@��@��@�@dZ@"�@�!@~�@~�@~�@~�@^5@^5@M�@J@�#@�^@��@x�@7L@Ĝ@�@ �@b@�;@�P@|�@l�@l�@K�@;d@
=@��@ȴ@��@�+@ff@$�@�h@`B@/@�@�@�@�@�D@j@1@�F@��@t�@S�@o@o@
�H@
�!@
n�@
^5@
=q@	�#@	��@	�7@	hs@	X@	X@	7L@	&�@	&�@	%@��@�@1'@b@��@��@\)@+@�@�@��@�y@�R@�+@ff@5?@$�@5?@$�@@@�@�-@��@�h@p�@?}@�@�@��@(�@�m@ƨ@�F@��@��@�@33@@�@�H@�H@��@��@��@�!@�\@~�@M�@-@J@�@��@�^@�7@X@�@ ��@ �`@ Ĝ@ �9@ ��@ �@ �@ �@ bN@ A�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B"�B"�B"�B#�B"�B"�B"�B"�B!�B�BVB��B�B�`B�`B�`B�`B�fB�sB�mB�mB�fB�fB�mB�fB�ZB�ZB�TB�HB�B��BŢB�FB��B�7Bp�BZBH�B<jB49B1'B#�B�B%BDB$�BJB%B1B��B�B�HB��BÖB�?B��B��B��B�DB�B~�B{�Bs�Be`BZBI�B2-B!�B�BVBB
�B
�yB
�ZB
�5B
��B
ȴB
�dB
�B
��B
�oB
�VB
�JB
�+B
w�B
iyB
]/B
K�B
2-B
"�B
�B
�B
�B
�B
{B
JB	��B	�B	�B	�`B	�BB	�#B	��B	��B	ǮB	ĜB	ÖB	��B	�^B	�B	�B	�B	��B	��B	��B	�VB	�1B	�B	�B	�B	�B	� B	}�B	y�B	v�B	t�B	r�B	m�B	hsB	aHB	XB	Q�B	Q�B	P�B	M�B	F�B	D�B	B�B	D�B	A�B	?}B	=qB	;dB	:^B	8RB	6FB	49B	.B	+B	)�B	'�B	$�B	�B	�B	�B	�B	oB	bB	VB	DB	+B��B�B�sB�mB�`B�ZB�ZB�NB�BB�)B��B��B��B��B��BɺBǮBŢB��B�jB�LB�9B�3B�-B�!B�B�B��B��B��B��B�{B�oB�hB�bB�\B�VB�PB�DB�7B�+B�B�B~�B|�Bz�Bx�Bu�Bt�Bs�Br�Bp�Bo�Bm�Bk�BjBiyBhsBgmBffBcTBaHB^5B]/B[#BYBVBQ�BO�BM�BK�BJ�BH�BG�BE�BD�BB�B?}B<jB:^B8RB7LB5?B33B1'B/B.B-B-B-B,B+B,B+B)�B'�B$�B#�B#�B"�B"�B$�B&�B%�B"�B �B �B �B�B�B �B �B�B�B!�B"�B#�B$�B%�B&�B'�B&�B'�B+B+B+B,B-B/B/B0!B1'B1'B2-B49B49B33B33B49B5?B5?B7LB7LB7LB9XB:^B;dB;dB:^B:^B9XB9XB9XB7LB7LB8RB9XB;dB@�BE�BG�BJ�BM�BN�BO�BP�BP�BP�BO�BO�BN�BL�BL�BK�BK�BL�BL�BN�BQ�BT�BVBXBYB^5B_;B_;B_;B_;B^5B`BBbNBdZBffBiyBk�Bm�Bp�Bq�Bq�Br�Bs�Bu�Bw�Bw�Bz�B|�B}�B�B�+B�7B�DB�VB�bB�oB��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�-B�?B�FB�LB�LB�LB�LB�LB�^B�wB��B��BŢB��B��B��B��B��B��B��B��B��B��B�/B�HB�NB�ZB�mB�sB�B�B�B�B��B��B��B��B��B��B	B	B	JB	\B	bB	hB	oB	�B	�B	�B	�B	�B	�B	 �B	!�B	!�B	#�B	&�B	(�B	,B	-B	1'B	33B	49B	5?B	5?B	7LB	;dB	=qB	>wB	B�B	C�B	E�B	H�B	I�B	L�B	N�B	P�B	R�B	T�B	VB	W
B	XB	YB	[#B	\)B	]/B	_;B	aHB	bNB	dZB	e`B	ffB	ffB	ffB	ffB	hsB	iyB	jB	jB	n�B	o�B	p�B	t�B	w�B	y�B	z�B	{�B	|�B	� B	�B	�B	�B	�B	�%B	�7B	�=B	�=B	�PB	�bB	�hB	�hB	�hB	�hB	�oB	�uB	�uB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�3B	�3B	�?B	�FB	�LB	�RB	�XB	�dB	�dB	�dB	�wB	�}B	�}B	��B	��B	B	ĜB	ŢB	ƨB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�5B	�;B	�;B	�;B	�BB	�HB	�NB	�NB	�NB	�TB	�TB	�ZB	�ZB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
%B
%B
+B
+B
+B
+B
+B
+B
+B
+B
1B
1B
1B
1B
	7B

=B
DB
JB
JB
JB
PB
VB
VB
VB
VB
VB
VB
VB
\B
bB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
,B
-B
-B
-B
.B
.B
.B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
2-B
33B
33B
33B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
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
:^B
;dB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
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
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
G�B
H�B
H�B
H�B
I�B
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
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
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
W
B
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
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
]/B
^5B
^5B
^5B
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
bNB
bNB
bNB
bNB
bNB
bNB
bNB
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
e`B
ffB
ffB
ffB
gmB
gmB
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
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
n�B
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
p�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B"�B"�B"�B#�B"�B"�B#B#TB#B�B�B 4B�TB�B�zB�`B�zB�fB�sB�B�B�B�B�B��B��B�FB�ZB��B�	B�gBʦB�VB�2B��By�B`'BL~B?�B8�B5�B%�BWBfB�B'�B�B�B
�B��B�CB��B�:B�tB��B��B��B�B�dB��B�B}VButBgRB]BMB4B# BB�B�B
��B
�B
��B
�B
ӏB
��B
�B
�]B
�IB
�&B
��B
��B
��B
zDB
l"B
`�B
P.B
4�B
#�B
!B
/B
=B
�B
mB
vB	��B	��B	�UB	�B	�|B	��B	�{B	̘B	�1B	�B	�gB	�aB	��B	��B	�qB	��B	��B	�@B	��B	��B	��B	��B	��B	��B	�UB	��B	~�B	z�B	w�B	u�B	s�B	o5B	jB	c�B	YB	RTB	RoB	Q�B	O�B	GzB	EmB	CGB	E�B	BuB	@B	>(B	;�B	:�B	8�B	72B	6+B	/B	+�B	*�B	(�B	&2B	 �B	7B	_B	B	[B	4B	�B	�B		�B	 �B�B�DB�
B��B��B��B�TB�NB޸B֡BϑB�pB�0B�xBʦB��B�EB��B��B�B��B��B��B�B� B�]B��B��B��B��B�2B��B��B�NB�B�B�B�B�#B�KB�tB�aB�B}�B|By�Bv`ButBt�BshBq�Bp�Bn}Bl=BkBj0Bi*Bh>Bg�BeBbNB_;B^5B\]BZ�BX_BS&BQBN�BL�BK�BJrBH�BFtBE�BC�BA�B=�B;�B9�B8�B6�B4�B2�B0�B.�B-�B-�B-�B,�B,=B-B,"B+�B)�B&B$tB$�B#�B#�B%�B'�B'8B$�B"NB!HB!B BB vB!�B!�B�B vB#TB$�B%B%�B&�B'�B(�B'�B(�B+6B+QB+�B-)B./B/�B/�B0�B1�B1�B2�B4�B4�B3�B3�B4�B5�B6FB7�B7�B88B9�B:�B;�B;�B;B;0B:�B:�B;B8B7�B8�B9�B;�B@�BFtBH�BKDBNVBO\BP}BQ�BQ4BQNBPbBP}BO�BMPBMjBL~BLdBMjBM�BPBR�BU�BV�BX�BY�B^�B_pB_VB_�B_�B_B`�Bb�Bd�Bf�Bi�Bk�BnBp�Bq�BrBsMBtnBvFBxRBxRB{JB}qB~�B��B��B��B��B��B��B�B�9B�B�B�CB�5B�'B�vB��B�mB�eB�kB�WB�wB�oB��B��B��B��B��B��B��B��B��B��B��B�B�B��B�B�BB�vB�\B�"B�HB�:B�uBյB�~B�B�B��B��B��B��B��B�=B�-B��B�B�$B�*B�JB�wB	�B	�B	~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	!B	"B	"B	$@B	'RB	)_B	,=B	-]B	1[B	3�B	4�B	5tB	5�B	7�B	;�B	=�B	>�B	B�B	C�B	E�B	H�B	J	B	MB	OBB	QNB	S&B	U2B	V9B	W?B	X_B	YeB	[WB	\xB	]~B	_�B	a|B	b�B	d�B	e�B	f�B	f�B	f�B	f�B	h�B	i�B	j�B	j�B	n�B	o�B	qB	uB	xB	zB	z�B	|B	|�B	� B	�;B	�aB	�SB	�MB	�YB	�lB	�rB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�-B	�TB	�2B	�
B	�B	�6B	�WB	�CB	�UB	�GB	�hB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�"B	�B	�.B	�4B	�TB	�2B	�?B	�KB	�KB	�KB	�QB	�QB	�QB	�=B	�]B	�xB	�jB	�pB	�pB	�pB	�vB	�|B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�$B	�B	�B	��B	�B	�B	�B	�(B	�.B
 4B
GB
9B
SB
YB
?B
EB
EB
EB
EB
EB
EB
EB
EB
KB
fB
fB
fB
	�B

�B
xB
~B
~B
~B
�B
�B
�B
�B
pB
pB
pB
�B
\B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
"�B
#B
#�B
$B
$B
%B
%B
%B
&B
&B
&B
&B
'B
'8B
)*B
)*B
)*B
*0B
*0B
*0B
*0B
+6B
+6B
+6B
,=B
-CB
-]B
-]B
.IB
.IB
.IB
/OB
/OB
0UB
0;B
0;B
0;B
0UB
1[B
2aB
3MB
3hB
3hB
4nB
4nB
4nB
5tB
5tB
6`B
6FB
6`B
6zB
6�B
6�B
7�B
7�B
8�B
8lB
8lB
8�B
8�B
8�B
9�B
9�B
:�B
;B
<�B
<�B
<�B
=�B
=�B
=�B
>�B
>�B
?�B
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
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
G�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
LB
MB
MB
M�B
M�B
NB
OB
OB
OB
OB
PB
O�B
QB
QB
QB
R B
R B
RB
R:B
S&B
SB
TB
TB
T,B
T,B
TB
T,B
UB
U2B
V9B
V9B
V9B
W?B
W?B
W?B
W?B
WYB
X_B
YeB
YKB
YKB
Y1B
YKB
YKB
Y1B
YKB
ZQB
[WB
[=B
[=B
[WB
[WB
\CB
\]B
\]B
]IB
^jB
^jB
^OB
_pB
_pB
`vB
`vB
`vB
abB
abB
abB
aHB
abB
bhB
bhB
bNB
b�B
bhB
b�B
bhB
c�B
dtB
dtB
dtB
dtB
dtB
dtB
d�B
d�B
d�B
d�B
d�B
d�B
e�B
f�B
f�B
f�B
g�B
g�B
gmB
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
n�B
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
p�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
xB
y	B
x�B
x�B
x�B
y	B
x�B
zB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
zB
y�B
zB
zB
y�B
z�B
{B
z�B
{B
{B
|B
|B
}B
}"B
}B
}B
}B
}B
}B
}"B
}"B
}111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.17(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201912100033372019121000333720191210003337202211182141072022111821410720221118214107201912111048062019121110480620191211104806  JA  ARFMdecpA19c                                                                20191130003712  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191129153716  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20191129153718  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191129153719  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191129153720  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20191129153720  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191129153720  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20191129153720  QCF$                G�O�G�O�G�O�            8000JA  ARGQaqcp2.8e                                                                20191129153720  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20191129153720  QCF$                G�O�G�O�G�O�            8000JA      jafc1.0                                                                 20191129153721                      G�O�G�O�G�O�                JA  ARUP                                                                        20191129155419                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20191204024635  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQJMQC2.0                                                                 20191204024606  CV  JULD            G�O�G�O�F�|�                JM  ARCAJMQC2.0                                                                 20191209153337  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191209153337  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20191211014806  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118124107  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                