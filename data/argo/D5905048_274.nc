CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-08-24T00:38:56Z creation;2018-08-24T00:39:15Z conversion to V3.1;2019-12-19T07:30:29Z update;     
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
resolution        =���   axis      Z        `  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \4   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ol   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  sD   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �|   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ˬ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ۜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180824003856  20200116231517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0577_274                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�{��>3 1   @�{���b�@4pѷX��db�,<��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CQ�fCT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU�fDV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@5�@u�@��\@��\AG�A=G�A]G�A}G�A���A���A���A���AΣ�Aޣ�A��A���BQ�BQ�BQ�BQ�B'Q�B/Q�B7Q�B?Q�BGQ�BOQ�BWQ�B_Q�BgQ�BoQ�BwQ�BQ�B���B���B���B���B���B���B���B���B���B���B���B�u�B���B���B���B���Bè�BǨ�B˨�BϨ�BӨ�Bר�Bۨ�Bߨ�B��B��B��B��B��B���B���B���C�{C�{C�{C�{C	�{C�{C�{C�{C�{C�C�{C�{C�{C�{C�{C�{C!�{C#�{C%�{C'�{C)�{C+�{C-�{C/�{C1�{C3�{C5�{C7�{C9�{C;�{C=�{C?�{CA�{CC�{CE�{CG�{CI�{CK�{CM�{CO�{CQ��CS�{CU�{CW�{CY�{C[�{C]�{C_�{Ca�{Cc�{Ce�{Cg�{Ci�{Ck�{Cm�{Co�{Cq�{Cs�{Cu�{Cw�{Cy�{C{�{C}�{C�{C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��
C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=D uD �DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D	uD	�D
uD
�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D uD �D!uD!�D"uD"�D#uD#�D$uD$�D%uD%�D&uD&�D'uD'�D(uD(�D)uD)�D*uD*�D+uD+�D,uD,�D-uD-�D.uD.�D/uD/�D0uD0�D1uD1�D2uD2�D3uD3�D4uD4�D5uD5�D6uD6�D7uD7�D8uD8�D9uD9�D:uD:�D;uD;�D<uD<�D=uD=�D>uD>�D?uD?�D@uD@�DAuDA�DBuDB�DCuDC�DDuDD�DEuDE�DFuDF�DGuDG�DHuDH�DIuDI�DJuDJ�DKuDK�DLuDL�DMuDM�DNuDN�DOuDO�DPuDP�DQuDQ�DRuDR�DSuDS�DTuDT�DU{�DU�DVuDV�DWuDW�DXuDX�DYuDY�DZuDZ�D[uD[�D\uD\�D]uD]�D^uD^�D_uD_�D`uD`�DauDa�DbuDb�DcuDc�DduDd�DeuDe�DfuDf�DguDg�DhuDh�DiuDi�DjuDj�DkuDk�DluDl�DmuDm�DnuDn�DouDo�DpuDp�DquDq�DruDr�DsuDs�DtuDt�DuuDu�DvuDv�DwuDw�DxuDx�DyuDy�DzuDz�D{uD{�D|uD|�D}uD}�D~uD~�DuD�D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D��\D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�DຏD���D�:�D�z�DẏD���D�:�D�z�D⺏D���D�:�D�z�D㺏D���D�:�D�z�D亏D���D�:�D�z�D序D���D�:�D�z�D溏D���D�:�D�z�D纏D���D�:�D�z�D躏D���D�:�D�w\D麏D���D�:�D�z�D꺏D���D�:�D�z�D뺏D���D�:�D�z�D캏D���D�:�D�z�D���D���D�:�D�z�DD���D�:�D�z�DﺏD���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D���D���D�:�D�z�D���D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���AܶFAܬAܬAܣ�Aܥ�Aܡ�Aܟ�Aܛ�Aܟ�Aܟ�Aܟ�Aܡ�Aܟ�Aܡ�Aܡ�Aܡ�Aܝ�Aܙ�Aܗ�AܓuA܅A�"�A�l�A���Aڧ�A�dZA�9XA�1A���A��`A���Aٴ9A�  A�K�A���Aק�A�VA�A�A���A�r�A�p�A�VA�33Aћ�A�hsA���A��A̗�A���Aʟ�A��`A�r�A�-A�ȴA�C�A��A�bNA��HA�bNA��A��mA���A�{A�ƨA��yA�A��+A��-A���A�%A���A���A�$�A�1A���A�ZA�A��;A���A�ĜA��A���A� �A���A�1'A�\)A�%A�bA�  A�;dA�+A��A�x�A�I�A��7A��A�7LA�bA�G�A��yA�\)A���A��yA�VA�\)A���A��HA���A���A�;dA�oA��A��`A� �A�S�A���A�ZA�hsA�|�A�K�A�ffA�ȴA���A���A33Azn�ArI�Ak�Ah�DAe�Ad��AcS�Aa�
A`�uA_C�A\ȴAZ�!AY+AX$�AW�AUAU%ATQ�AS��AQK�AN�HAJĜAG�AE�ADn�AC;dAA/A?��A=�A;�^A9oA8JA7C�A5hsA5�A4��A4~�A3�A1|�A/ƨA.��A.$�A-��A-C�A,ȴA,v�A,5?A+�A*�jA)�^A)7LA(��A(A�A'�;A%��A$�A#�A ��A I�A\)AbNA{A�A��AȴA�TA^5AXA-A��A^5A�#A;dAQ�A��A
I�A��AĜA1AA�PAG�A��AffA�A�hA/A��AQ�A�@��F@�\)@�O�@��u@��m@�E�@�@�ff@��@�C�@���@�j@�b@�v�@�@�Z@�P@��H@���@�Z@���@���@��@�v�@�z�@�dZ@�x�@ۍP@��y@�ff@��@���@�=q@պ^@�x�@�O�@��@�V@���@�Q�@��@��@щ7@���@�v�@�-@���@�G�@��@�l�@ʗ�@��@�p�@��`@��@��
@�C�@�ȴ@�-@��@�G�@ċD@Ý�@�@�@���@�X@�r�@�  @�K�@�V@�7L@�ƨ@�"�@��@��@��!@�ff@��@�?}@�Z@�
=@�-@���@�Z@��
@�C�@���@�ff@�O�@�z�@�A�@��
@��@��@�@�G�@��/@��j@�Z@�b@��F@�\)@�@��\@�V@�5?@���@���@�7L@��j@��D@�z�@�Z@�ƨ@�\)@�o@�^5@�~�@���@�5?@���@��@�Q�@�  @�K�@��@��@�v�@�E�@�{@��#@���@�x�@�%@�Ĝ@��@�j@�Z@�9X@���@���@�|�@�t�@�S�@�C�@�+@��y@�ff@���@��h@��@�x�@�hs@�O�@��@�%@�%@�%@�V@���@�Ĝ@��D@�(�@��
@��@���@��P@��P@���@�|�@�C�@��@�=q@�@�p�@�p�@�hs@�X@�X@�G�@�G�@�G�@��@��@��9@�z�@�A�@���@���@�\)@�S�@�33@��y@���@�v�@�ff@�-@��@�J@�J@��@��^@��7@�X@��@���@��@��u@�bN@�(�@��m@��@��P@�|�@�K�@�@�ȴ@���@�^5@�-@���@��@��@��#@���@�`B@��@���@�Ĝ@��D@�1'@���@�dZ@�;d@��!@�=q@�{@��-@�&�@��/@�r�@���@��@�l�@�
=@��y@��!@���@�E�@��@���@�&�@�Ĝ@��j@��@�I�@�(�@� �@��@�1@��;@��w@��@��P@�;d@���@��!@��+@�^5@�-@�{@�@��T@��T@���@��h@�X@�/@�&�@��@�V@��`@���@��D@�r�@�A�@�1'@�1'@�(�@��m@���@��@��@�dZ@�S�@�;d@�
=@���@���@�5?@��@��^@��-@�?}@�j@�(�@�w@�P@~�@~�+@~E�@}��@}`B@|�/@|Z@{�m@{t�@z��@z�!@z�!@y�@y�7@yG�@x��@x��@xbN@xb@w|�@w+@w
=@v�y@vv�@u�-@u�h@up�@u?}@u/@t�@t(�@s�F@sdZ@r�H@r~�@rn�@q��@q�^@qhs@qhs@qX@q&�@qX@q�7@q�7@q��@q��@q��@q�7@q7L@p��@o��@o;d@n�@n$�@m��@m�@l�/@l9X@k�
@k�
@k�F@k�@kdZ@k33@k@j�!@j-@iX@h�u@h �@g�@gl�@g
=@f�y@f�@f�+@f@eO�@eV@d��@dZ@d1@c��@c�m@c��@c@b��@b^5@a��@a%@`�u@`1'@_
=@^�y@^v�@^{@]�T@]�-@]`B@\�@\�@[ƨ@[dZ@Z�@Z��@ZM�@Z�@Y�#@Y&�@X�@XbN@XbN@Xr�@X�@Xr�@X �@W�w@W�P@W\)@W;d@W+@W
=@V�@Vȴ@Vv�@V$�@U�T@U�h@T�j@Tj@TI�@T�@S�
@St�@SS�@SS�@S"�@R��@R��@R~�@Rn�@R�@Q�^@Q�7@Qhs@QG�@Q%@P��@PQ�@O�w@O;d@O+@O+@O+@O+@O�@N��@N�@Nȴ@N��@Nv�@N$�@M��@M�@M�@MV@L��@Lz�@LZ@L�@K��@K�
@Kt�@J�!@J�@Jn�@J�@I�@I��@IX@I7L@I&�@I�@H��@H�9@H�@Hr�@HA�@H �@G��@GK�@G;d@G�@F�@F�+@F5?@F{@E��@Ep�@EO�@E/@DZ@C��@C��@C�m@C�m@Cƨ@Ct�@B�@B^5@BJ@A�@A�^@A��@AG�@@�9@?�;@?|�@?\)@>�y@>��@>ff@>{@=��@=�@=�@=`B@=V@<�/@<�@<I�@;�
@;"�@:�@:�H@:�\@:n�@:��@:M�@:�@97L@8��@8�9@8�u@8A�@8  @7�@7;d@6��@6E�@6{@5�T@5�T@5@5��@5�h@5`B@5V@4�/@4�j@4�j@4�D@4(�@41@3�m@3ƨ@3�@2��@2��@2^5@2J@1�^@1x�@1G�@1%@0�u@0r�@0A�@0  @/��@/�@/�P@/l�@/
=@.�@.�+@.V@.E�@.$�@-�T@-�h@-�@-O�@,��@,�D@,j@,j@,Z@+�
@+�@+33@*��@*-@)�#@)�^@)hs@)�@(��@(r�@(bN@(Q�@(A�@(A�@(1'@(  @'�@'�P@'+@'�@&��@&��@&ff@&E�@&5?@&@%�@%�T@%�h@%`B@%/@$��@$�D@$I�@$�@#�F@#�@#t�@#S�@#o@"�@"��@"��@"^5@"-@!�#@!��@!�7@!G�@!7L@ ��@ �9@ �@ bN@ A�@   @��@K�@��@v�@V@{@@p�@O�@��@z�@I�@(�@1@�m@�
@ƨ@��@�@dZ@33@"�@@�@�H@��@�\@n�@-@��@hs@%@��@Ĝ@Ĝ@�u@�u@r�@A�@b@�;@��@�@��@l�@+@�y@v�@E�@{@{@�@@��@`B@�@�/@��@�@��@j@(�@��@�m@�
@ƨ@ƨ@ƨ@�F@��@��@�@t�@S�@33@@��@�!@~�@=q@-@�@�@�@�#@��@�^@�^@��@��@x�@hs@G�@7L@&�@�@�`@Ĝ@�9@�9@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���AܶFAܬAܬAܣ�Aܥ�Aܡ�Aܟ�Aܛ�Aܟ�Aܟ�Aܟ�Aܡ�Aܟ�Aܡ�Aܡ�Aܡ�Aܝ�Aܙ�Aܗ�AܓuA܅A�"�A�l�A���Aڧ�A�dZA�9XA�1A���A��`A���Aٴ9A�  A�K�A���Aק�A�VA�A�A���A�r�A�p�A�VA�33Aћ�A�hsA���A��A̗�A���Aʟ�A��`A�r�A�-A�ȴA�C�A��A�bNA��HA�bNA��A��mA���A�{A�ƨA��yA�A��+A��-A���A�%A���A���A�$�A�1A���A�ZA�A��;A���A�ĜA��A���A� �A���A�1'A�\)A�%A�bA�  A�;dA�+A��A�x�A�I�A��7A��A�7LA�bA�G�A��yA�\)A���A��yA�VA�\)A���A��HA���A���A�;dA�oA��A��`A� �A�S�A���A�ZA�hsA�|�A�K�A�ffA�ȴA���A���A33Azn�ArI�Ak�Ah�DAe�Ad��AcS�Aa�
A`�uA_C�A\ȴAZ�!AY+AX$�AW�AUAU%ATQ�AS��AQK�AN�HAJĜAG�AE�ADn�AC;dAA/A?��A=�A;�^A9oA8JA7C�A5hsA5�A4��A4~�A3�A1|�A/ƨA.��A.$�A-��A-C�A,ȴA,v�A,5?A+�A*�jA)�^A)7LA(��A(A�A'�;A%��A$�A#�A ��A I�A\)AbNA{A�A��AȴA�TA^5AXA-A��A^5A�#A;dAQ�A��A
I�A��AĜA1AA�PAG�A��AffA�A�hA/A��AQ�A�@��F@�\)@�O�@��u@��m@�E�@�@�ff@��@�C�@���@�j@�b@�v�@�@�Z@�P@��H@���@�Z@���@���@��@�v�@�z�@�dZ@�x�@ۍP@��y@�ff@��@���@�=q@պ^@�x�@�O�@��@�V@���@�Q�@��@��@щ7@���@�v�@�-@���@�G�@��@�l�@ʗ�@��@�p�@��`@��@��
@�C�@�ȴ@�-@��@�G�@ċD@Ý�@�@�@���@�X@�r�@�  @�K�@�V@�7L@�ƨ@�"�@��@��@��!@�ff@��@�?}@�Z@�
=@�-@���@�Z@��
@�C�@���@�ff@�O�@�z�@�A�@��
@��@��@�@�G�@��/@��j@�Z@�b@��F@�\)@�@��\@�V@�5?@���@���@�7L@��j@��D@�z�@�Z@�ƨ@�\)@�o@�^5@�~�@���@�5?@���@��@�Q�@�  @�K�@��@��@�v�@�E�@�{@��#@���@�x�@�%@�Ĝ@��@�j@�Z@�9X@���@���@�|�@�t�@�S�@�C�@�+@��y@�ff@���@��h@��@�x�@�hs@�O�@��@�%@�%@�%@�V@���@�Ĝ@��D@�(�@��
@��@���@��P@��P@���@�|�@�C�@��@�=q@�@�p�@�p�@�hs@�X@�X@�G�@�G�@�G�@��@��@��9@�z�@�A�@���@���@�\)@�S�@�33@��y@���@�v�@�ff@�-@��@�J@�J@��@��^@��7@�X@��@���@��@��u@�bN@�(�@��m@��@��P@�|�@�K�@�@�ȴ@���@�^5@�-@���@��@��@��#@���@�`B@��@���@�Ĝ@��D@�1'@���@�dZ@�;d@��!@�=q@�{@��-@�&�@��/@�r�@���@��@�l�@�
=@��y@��!@���@�E�@��@���@�&�@�Ĝ@��j@��@�I�@�(�@� �@��@�1@��;@��w@��@��P@�;d@���@��!@��+@�^5@�-@�{@�@��T@��T@���@��h@�X@�/@�&�@��@�V@��`@���@��D@�r�@�A�@�1'@�1'@�(�@��m@���@��@��@�dZ@�S�@�;d@�
=@���@���@�5?@��@��^@��-@�?}@�j@�(�@�w@�P@~�@~�+@~E�@}��@}`B@|�/@|Z@{�m@{t�@z��@z�!@z�!@y�@y�7@yG�@x��@x��@xbN@xb@w|�@w+@w
=@v�y@vv�@u�-@u�h@up�@u?}@u/@t�@t(�@s�F@sdZ@r�H@r~�@rn�@q��@q�^@qhs@qhs@qX@q&�@qX@q�7@q�7@q��@q��@q��@q�7@q7L@p��@o��@o;d@n�@n$�@m��@m�@l�/@l9X@k�
@k�
@k�F@k�@kdZ@k33@k@j�!@j-@iX@h�u@h �@g�@gl�@g
=@f�y@f�@f�+@f@eO�@eV@d��@dZ@d1@c��@c�m@c��@c@b��@b^5@a��@a%@`�u@`1'@_
=@^�y@^v�@^{@]�T@]�-@]`B@\�@\�@[ƨ@[dZ@Z�@Z��@ZM�@Z�@Y�#@Y&�@X�@XbN@XbN@Xr�@X�@Xr�@X �@W�w@W�P@W\)@W;d@W+@W
=@V�@Vȴ@Vv�@V$�@U�T@U�h@T�j@Tj@TI�@T�@S�
@St�@SS�@SS�@S"�@R��@R��@R~�@Rn�@R�@Q�^@Q�7@Qhs@QG�@Q%@P��@PQ�@O�w@O;d@O+@O+@O+@O+@O�@N��@N�@Nȴ@N��@Nv�@N$�@M��@M�@M�@MV@L��@Lz�@LZ@L�@K��@K�
@Kt�@J�!@J�@Jn�@J�@I�@I��@IX@I7L@I&�@I�@H��@H�9@H�@Hr�@HA�@H �@G��@GK�@G;d@G�@F�@F�+@F5?@F{@E��@Ep�@EO�@E/@DZ@C��@C��@C�m@C�m@Cƨ@Ct�@B�@B^5@BJ@A�@A�^@A��@AG�@@�9@?�;@?|�@?\)@>�y@>��@>ff@>{@=��@=�@=�@=`B@=V@<�/@<�@<I�@;�
@;"�@:�@:�H@:�\@:n�@:��@:M�@:�@97L@8��@8�9@8�u@8A�@8  @7�@7;d@6��@6E�@6{@5�T@5�T@5@5��@5�h@5`B@5V@4�/@4�j@4�j@4�D@4(�@41@3�m@3ƨ@3�@2��@2��@2^5@2J@1�^@1x�@1G�@1%@0�u@0r�@0A�@0  @/��@/�@/�P@/l�@/
=@.�@.�+@.V@.E�@.$�@-�T@-�h@-�@-O�@,��@,�D@,j@,j@,Z@+�
@+�@+33@*��@*-@)�#@)�^@)hs@)�@(��@(r�@(bN@(Q�@(A�@(A�@(1'@(  @'�@'�P@'+@'�@&��@&��@&ff@&E�@&5?@&@%�@%�T@%�h@%`B@%/@$��@$�D@$I�@$�@#�F@#�@#t�@#S�@#o@"�@"��@"��@"^5@"-@!�#@!��@!�7@!G�@!7L@ ��@ �9@ �@ bN@ A�@   @��@K�@��@v�@V@{@@p�@O�@��@z�@I�@(�@1@�m@�
@ƨ@��@�@dZ@33@"�@@�@�H@��@�\@n�@-@��@hs@%@��@Ĝ@Ĝ@�u@�u@r�@A�@b@�;@��@�@��@l�@+@�y@v�@E�@{@{@�@@��@`B@�@�/@��@�@��@j@(�@��@�m@�
@ƨ@ƨ@ƨ@�F@��@��@�@t�@S�@33@@��@�!@~�@=q@-@�@�@�@�#@��@�^@�^@��@��@x�@hs@G�@7L@&�@�@�`@Ĝ@�9@�9@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	ŢB	ǮB	ȴB	ɺB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ȴB	ǮB	ĜB	��B	�?B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�{B	��B	��B	��B	�!B	�RB	�B	�B	�jB	��B	�B	��B
B
D�B
S�B
�oB
�hB
��B
��B
�B
�ZB
�yB
��B
��B
=BbBJB2-BQ�BC�B=qB�JB��B��B��B�B�sB��B+B!�B7LBE�B6FB��B��B�`B��B�sBŢB��B�B��B�B�sB�NB��B�RB��B��B�^BɺB�B�B	7B��B��B��B�B�;B��B��B��B��B��B�B_;B)�B&�BVBB
��B
�B
�B
ƨB
��B
cTB
N�B
ZB
F�B
+B	�/B	�^B	�B	"�B�B�`B�B�B�B�HB�)B��B�}BBƨB��B��B��B�#B�)B�B��B�3B�hB�\B��B��B��B�{B��B�bB�JB�+B��B��B��B��B�B��B��B��B�^BƨB��B�B�B�B�B�
B��B��B��B�B�B��B��B��BB��B�-B�9B�9B��B�{B��B��B�uB�JB�B�B~�B|�B~�B|�Be`Bu�Bx�Bm�B`BBXBk�Bu�Bw�Bw�Bu�Bs�Bo�Bx�Bs�Bo�BiyB_;BVBL�B^5Be`Be`B^5BVB`BBdZB[#B`BBgmBjBcTBk�Be`Bl�Bm�Bm�Bl�Bu�Bu�By�Bx�Bn�Bt�Bv�B{�B�VB�bB�PB�PB�hB��B��B��B��B��B��B��B�uB��B��B�hB��B��B��B��B��B��B��B��B�B�!B�!B�LB�FB�LB�LB�jB�^B�XB�dB�}BŢBǮBȴBBƨBĜBÖBÖBŢB��B�B�)B�/B�/B�)B�;B�;B�NB�B�sB��B�B�B��B��B��B��B��B	B	B	%B	B	JB	uB	�B	�B	�B	�B	#�B	%�B	'�B	,B	.B	-B	2-B	6FB	9XB	@�B	F�B	J�B	H�B	K�B	L�B	J�B	T�B	W
B	VB	R�B	YB	^5B	^5B	_;B	jB	k�B	k�B	q�B	t�B	v�B	x�B	{�B	{�B	�B	�B	�1B	�7B	�7B	�7B	�=B	�PB	�\B	�\B	�bB	�bB	�bB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�-B	�'B	�!B	�B	�B	�'B	�?B	�^B	�^B	�^B	�^B	�^B	�^B	�^B	�XB	�XB	�XB	�^B	�dB	�dB	�wB	��B	B	B	B	ÖB	ƨB	ȴB	ǮB	ɺB	��B	��B	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�/B	�)B	�#B	�5B	�/B	�5B	�BB	�HB	�HB	�ZB	�ZB	�fB	�`B	�ZB	�fB	�`B	�fB	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
B
B
B
B
B
B
B
B
B
+B
+B
B
%B
+B
+B
1B
1B
1B
1B
	7B
JB
DB

=B
1B
JB
PB
PB
PB
PB
PB
VB
VB
VB
VB
bB
\B
bB
hB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
"�B
"�B
"�B
"�B
"�B
!�B
 �B
 �B
!�B
"�B
#�B
$�B
$�B
&�B
&�B
%�B
$�B
#�B
%�B
&�B
%�B
&�B
(�B
(�B
'�B
&�B
)�B
(�B
'�B
'�B
(�B
)�B
&�B
,B
+B
,B
.B
-B
,B
+B
,B
-B
-B
-B
.B
.B
/B
/B
.B
/B
1'B
49B
49B
49B
49B
49B
33B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
5?B
5?B
5?B
5?B
49B
7LB
8RB
7LB
7LB
7LB
8RB
9XB
9XB
8RB
9XB
9XB
9XB
8RB
7LB
9XB
9XB
:^B
9XB
:^B
8RB
9XB
9XB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
:^B
:^B
9XB
:^B
:^B
;dB
=qB
<jB
=qB
>wB
>wB
>wB
>wB
=qB
=qB
=qB
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
B�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
D�B
E�B
E�B
C�B
F�B
H�B
H�B
G�B
G�B
F�B
E�B
E�B
F�B
G�B
G�B
H�B
F�B
E�B
E�B
G�B
H�B
G�B
H�B
H�B
I�B
I�B
H�B
L�B
L�B
K�B
L�B
L�B
K�B
J�B
I�B
K�B
K�B
K�B
K�B
N�B
L�B
L�B
K�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
Q�B
S�B
S�B
T�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
S�B
T�B
T�B
T�B
S�B
R�B
VB
VB
VB
VB
VB
W
B
W
B
VB
XB
XB
XB
XB
YB
YB
YB
XB
XB
YB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
ZB
ZB
[#B
\)B
[#B
ZB
[#B
[#B
[#B
ZB
]/B
]/B
]/B
]/B
]/B
^5B
_;B
_;B
_;B
_;B
_;B
^5B
^5B
^5B
^5B
`BB
_;B
_;B
`BB
`BB
aHB
`BB
aHB
aHB
`BB
`BB
`BB
`BB
`BB
aHB
bNB
aHB
bNB
cTB
bNB
bNB
cTB
cTB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
dZB
dZB
e`B
e`B
e`B
dZB
dZB
dZB
dZB
gmB
gmB
ffB
ffB
gmB
gmB
ffB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
iyB
jB
jB
jB
jB
jB
jB
iyB
jB
iyB
iyB
iyB
jB
l�B
l�B
m�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
l�B
l�B
l�B
l�B
m�B
n�B
o�B
n�B
n�B
n�B
n�B
n�B
p�B
o�B
o�B
o�B
o�B
o�B
p�B
q�B
q�B
r�B
r�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
s�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
u�B
u�B
u�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�+B	�NB	��B	�B	�B	��B	�	B	��B	��B	��B	�=B	��B	�B	��B	��B	�FB	��B	�	B	��B	�}B	��B	�@B	�MB	��B
�B
F�B
W$B
��B
��B
��B
��B
�B
�,B
�B
��B
�VB�BBBB3�BRTBFBBB�"B��B�:B��B�B��B��B1B"�B7�BFtB8RB�BB֡B��B�+B�KB��B��B��B�(B��B�B��BŢB�*B�}B��B�B�	B��B)BB��B�}B��B�B�BΊB�B��B��B�B�mBb�B/�B+�B�B�B
��B
�B
�CB
ʦB
�B
i�B
S�B
\�B
KDB
"B	�B	�]B	��B	,=B��B�B�}B�B�oB�TB�B�
BªB�BȚB�HB�mBөB�)B�IBۦB��B��B��B�B��B��B��B�YB��B�&B�B�rB�B�2B��B�B��B�B��B�B�PB�BյB��B��BخBخBרB��B�BB�.BּB��B��B��B�-BāB̈́B�tB��B�+B��B�YB�kB�B��B��B�+B��B��B~�B�B~(Bh�Bw2Bz*Bo�Bb�BZkBl�BvFBx8BxlBvzBt�Bp�By	BtnBpoBj�BaBX+BO�B_�BfBf2B_�BW�Ba-Be,B\�BabBh$BkBd�Bl"Bf�BmCBnIBn}Bm�BvFBv�BzDByXBpBu�BxRB}<B��B�B�pB�pB��B�#B�B�B�B�B�B�KB��B�kB�xB�&B�B�&B�&B�pB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�6B�OB�B�B�B�GB�+B�9B�gBāB��B�hB�_B�CB�~BݘBܬB��B�B�TB�6B�B�B�9B�9B�FB�lB��B��B�VB	oB	�B	�B	B	�B	�B	�B		B	�B	 'B	$@B	&LB	(XB	,WB	.cB	-�B	2|B	6�B	9�B	@�B	F�B	KB	I7B	L0B	M6B	K^B	UB	W?B	VmB	S�B	YB	^�B	^�B	_�B	j�B	k�B	k�B	q�B	t�B	wB	y$B	|B	|PB	�UB	�MB	�KB	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�,B	�2B	�RB	�QB	�IB	�UB	�[B	�-B	�GB	�vB	�oB	��B	��B	�vB	��B	�xB	��B	�xB	�xB	��B	�xB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�	B	��B	�B	�B	�B	�B	�.B	�B	�4B	�:B	�&B	�,B	�@B	�@B	�,B	�2B	�MB	�9B	�9B	�+B	�?B	�?B	�MB	�2B	�YB	�EB	�_B	�EB	�yB	ևB	�eB	�kB	ؓB	چB	�~B	�xB	ۦB	ޞB	�~B	ޞB	��B	�B	�B	�tB	�B	�B	��B	��B	�B	��B	��B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�	B	�$B	��B	�B	�"B	�B	�B	�0B	�*B	�B	�(B	�"B	�(B	�B	�<B	�(B	�6B	�^B	�6B	�BB	�(B	�JB	�zB	�6B	�<B
 4B	�cB
;B
AB
[B
aB
AB
aB
gB
MB
gB
EB
EB
�B
tB
_B
_B
fB
fB
�B
fB
	lB
~B
xB

rB
�B
dB
�B
jB
jB
�B
�B
�B
�B
�B
�B
}B
�B
�B
�B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
�B
B
B
"�B
"�B
#B
#B
#B
#B
"B
 �B
!-B
"B
#B
$&B
%B
%,B
'B
'B
&2B
%,B
$@B
&B
'B
&2B
'B
(�B
)*B
($B
'8B
*0B
)*B
($B
(XB
)DB
*KB
'RB
,=B
+QB
,=B
.IB
-)B
,WB
+kB
,WB
-)B
-]B
-CB
.IB
.cB
/OB
/OB
.cB
/OB
1[B
49B
4TB
4TB
4TB
4nB
3�B
5ZB
5ZB
6zB
6`B
6zB
6zB
6zB
5tB
5ZB
5tB
5�B
4�B
7�B
8�B
7fB
7fB
7�B
8�B
9rB
9�B
8�B
9rB
9rB
9�B
8lB
7�B
9�B
9�B
:�B
9rB
:�B
8�B
9�B
9�B
;B
;B
;dB
;B
;B
;�B
;�B
;dB
:�B
:�B
9�B
:�B
:�B
;�B
=�B
<�B
=�B
>�B
>�B
>�B
>�B
=�B
=�B
=�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
B�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
D�B
E�B
E�B
C�B
F�B
H�B
H�B
G�B
G�B
F�B
E�B
E�B
F�B
G�B
G�B
H�B
F�B
E�B
FB
G�B
H�B
G�B
H�B
H�B
I�B
I�B
H�B
L�B
MB
K�B
L�B
L�B
LB
J�B
J	B
K�B
K�B
K�B
K�B
N�B
MB
L�B
L0B
M�B
OB
OB
OB
O�B
P.B
P.B
P.B
R B
T,B
T,B
UB
T,B
T,B
T,B
TB
TB
U2B
U2B
UB
U2B
T,B
U2B
U2B
U2B
T,B
S@B
VB
VB
V9B
V9B
VB
W?B
W?B
VSB
X+B
XEB
XEB
X+B
YKB
YKB
YKB
XEB
XEB
Y1B
ZQB
ZQB
ZQB
ZQB
Z7B
[WB
[WB
Z7B
ZQB
[WB
\CB
[WB
ZkB
[WB
[WB
[qB
Z�B
]dB
]dB
]dB
]dB
]~B
^jB
_VB
_VB
_VB
_VB
_VB
^jB
^jB
^OB
^jB
`\B
_VB
_pB
`vB
`vB
abB
`vB
aHB
a|B
`vB
`vB
`vB
`vB
`vB
a|B
b�B
a|B
b�B
cnB
b�B
b�B
cnB
c�B
b�B
c�B
c�B
c�B
d�B
d�B
d�B
e�B
d�B
d�B
e�B
ezB
e�B
d�B
d�B
d�B
d�B
g�B
g�B
f�B
f�B
g�B
g�B
f�B
h�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
i�B
j�B
j�B
j�B
jB
j�B
j�B
i�B
j�B
i�B
i�B
i�B
j�B
l�B
l�B
m�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
l�B
l�B
l�B
l�B
m�B
n�B
o�B
n�B
n�B
n�B
n�B
n�B
p�B
o�B
o�B
o�B
o�B
o�B
p�B
q�B
q�B
r�B
r�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
s�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
u�B
u�B
u�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.17(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201808280039292018082800392920180828003929201808280200282018082802002820180828020028201808290028412018082900284120180829002841  JA  ARFMdecpA19c                                                                20180824093609  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180824003856  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180824003859  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180824003903  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180824003904  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180824003904  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180824003905  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180824003905  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180824003911  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180824003915                      G�O�G�O�G�O�                JA  ARUP                                                                        20180824010241                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180824153420  CV  JULD            G�O�G�O�F�߰                JM  ARCAJMQC2.0                                                                 20180827153929  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180827153929  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180827170028  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180828152841  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231517                      G�O�G�O�G�O�                