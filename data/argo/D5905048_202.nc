CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-01-20T00:35:24Z creation;2018-01-20T00:35:28Z conversion to V3.1;2019-12-19T07:47:15Z update;     
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
resolution        =���   axis      Z        d  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  s\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �X   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �`   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �d   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �h   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180120003524  20200116221516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_202                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�E�A��1   @�E��r @3�}�H��dh�4֡b1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DE��DF� DG  DG� DH  DH� DI  DI� DJ  DJy�DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di�fDj  Dj� Dk  Dk� Dk��Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@��\@��\A�HA=G�A]G�A}G�A���A���A���A���AΣ�Aޣ�A��A���BQ�BQ�BQ�BQ�B'Q�B/Q�B7Q�B?Q�BGQ�BOQ�BWQ�B_Q�BgQ�BoQ�BwQ�BQ�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bè�BǨ�B˨�BϨ�BӨ�Bר�Bۨ�Bߨ�B��B��B��B��B��B���B���B���C�{C�{C�{C�{C	�{C�{C�{C�{C�{C��C�{C�{C�{C�{C�{C�{C!�{C#�{C%�{C'�{C)�{C+�{C-�{C/�{C1�{C3�{C5�{C7�{C9�{C;�{C=�{C?�{CA�{CC�{CE�{CG�{CI�{CK�{CM�{CO�{CQ�{CS�{CU�{CW�{CY�{C[�{C]�{C_�{Ca�{Cc�{Ce�{Cg�{Ci�{Ck�{Cm�{Co�{Cq�{Cs�{Cu�{Cw�{Cy�{C{�{C}�{C�{C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��
C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=D uD �DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D	uD	�D
uD
�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD��D uD �D!uD!�D"uD"�D#uD#�D$uD$�D%uD%�D&uD&�D'uD'�D(uD(�D)uD)�D*uD*�D+uD+�D,uD,�D-uD-�D.uD.�D/uD/�D0uD0�D1uD1�D2uD2�D3uD3�D4uD4�D5uD5�D6uD6�D7uD7�D8uD8�D9uD9�D:uD:�D;uD;�D<uD<�D=uD=�D>uD>�D?uD?�D@uD@�DAuDA�DBuDB�DCuDC�DDuDD�DEuDE�DFuDF�DGuDG�DHuDH�DIuDI�DJn�DJ�DKuDK�DLuDL�DMuDM�DNuDN�DOuDO�DPuDP�DQuDQ�DRuDR�DSuDS�DTuDT�DUuDU�DVuDV�DWuDW�DXuDX�DYuDY�DZuDZ�D[uD[�D\uD\�D]uD]�D^uD^�D_uD_�D`uD`�DauDa�DbuDb�DcuDc�DduDd�DeuDe�DfuDf�DguDg�DhuDh�Di{�Di�DjuDj�DkuDk�DluDl�DmuDm�DnuDn�DouDo�DpuDp�DquDq�DruDr�DsuDs�DtuDt�DuuDu�DvuDv�DwuDw�DxuDx�DyuDy�DzuDz�D{uD{�D|uD|�D}uD}�D~uD~�DuD�D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�=�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�DຏD���D�:�D�z�DẏD���D�:�D�z�D⺏D���D�:�D�z�D㺏D���D�:�D�z�D亏D���D�:�D�z�D序D���D�:�D�z�D溏D���D�:�D�z�D纏D���D�:�D�z�D躏D���D�:�D�z�D麏D���D�:�D�z�D꺏D���D�:�D�z�D뺏D���D�:�D�z�D캏D���D�:�D�z�D���D���D�:�D�z�DD���D�:�D�z�DﺏD���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D���D���D�:�D�z�D���D���D�=�D�}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��A�
=A��A��TA��
A���Aũ�Aş�Aŏ\Aŉ7A�hsA�G�A�=qA�1'A��A�{A�{A�bA�VA�bA�bA�JA�%A�%A�%A�%A�A���A���A���A��A��A��A��A��A��A��A��A��A��TA���AĬAě�A�t�A��AöFA��A���A¡�A�5?A���A���A�%A�G�A��A���A��A�VA��A��uA�A���A���A��!A�p�A�t�A��A�?}A���A��FA�&�A�dZA�A�K�A��A��-A�JA�jA���A��FA���A�9XA�A�|�A���A�VA�bNA���A���A�|�A�Q�A���A�hsA�=qA���A�E�A���A��A��A�XA�1'A��A��wA�-A��A�bNA��!A�M�A��/A�jA�9XA�bNA��jA�{A�t�A�
=A���A�7A{�wAz�Aw��Av��Av �As�wAq�AqhsAp��Ao��Am�Aj��Ail�Ah5?Af  AdZAbI�Aa��A_S�A]�wA\��A\E�A[��AY�ATI�AQ`BAP�DAOAM�TAM%AK�AI�AG�wAE�mAEG�ACx�AA�A?�
A=K�A;�A9�#A81'A7&�A5��A4 �A3S�A2�+A2JA1+A.��A.A-?}A,z�A+x�A*VA'|�A&A�A$�yA#�A"v�A!��A!p�A E�A�/A�7AZA�A �A`BAJAA�A�yA9XA;dA��AQ�A\)A�A�TA��A�AVA
�A
z�A
Q�A
{A	�PA��A�wA��A�\AAv�A33A��AI�AJA��A%A n�@��m@�9X@��9@�V@�hs@���@�h@���@�I�@�V@��@��@� �@�+@�$�@�D@�R@�;d@�v�@�O�@���@܋D@�~�@�bN@ם�@�ff@�X@�V@��/@Ԭ@�"�@��T@��@�S�@�ȴ@͙�@���@�9X@�ȴ@ə�@���@��@Ǯ@�o@ư!@���@�?}@���@ÍP@��#@�hs@��@��@�t�@��H@�~�@�$�@�`B@��9@�z�@�1'@���@�dZ@�o@���@�V@�-@�x�@��@���@��w@�;d@��@���@�~�@�-@��-@�O�@��@��`@��u@� �@��w@�+@���@���@�~�@�-@��@�@��7@�7L@���@���@�I�@���@��@��@�ȴ@���@�`B@��@���@�b@�ƨ@��P@�\)@�S�@�C�@�
=@�V@��@�G�@��@�1@��@��@��F@�\)@�C�@�
=@��y@��@��+@�=q@���@�%@���@���@�%@���@���@� �@�t�@�\)@��@�
=@�@���@�ȴ@�ȴ@���@���@�ff@�E�@�^5@�@�G�@�/@���@��`@��/@��j@��9@��@��u@��@��
@��F@�t�@�C�@��@��H@���@�n�@�5?@��@��@��@�x�@�O�@���@�Ĝ@��@���@�j@�A�@� �@���@��@�\)@��@��y@��+@�^5@�=q@�5?@�5?@��@���@�@���@�hs@���@��D@��@�r�@�Z@��
@��P@�t�@�;d@��H@��H@��@���@�@�@��@���@�~�@�^5@�M�@�5?@�@��7@�?}@�V@�%@��/@��9@���@�j@�I�@�(�@�  @��@�dZ@�S�@�C�@�33@���@���@���@�E�@�@��@�7L@���@��@��j@��u@�j@�b@�ƨ@�t�@�;d@���@�^5@���@�O�@�x�@�X@�%@��`@�Ĝ@���@��@��D@�I�@� �@��
@�dZ@�+@��H@�v�@�^5@�V@�V@�V@�V@�V@�J@��@��#@��h@�&�@���@�z�@�r�@�b@��w@��@�@��y@���@���@�M�@���@�O�@�%@���@��j@�z�@�I�@�  @��m@��w@���@�S�@�"�@���@�n�@�ff@�-@���@���@��7@�p�@�?}@�V@�%@��@���@���@��@��D@�A�@�;@\)@~��@}�@}`B@|��@|��@|Z@|I�@{�
@{ƨ@{��@{�@{S�@{33@z�H@zM�@y��@x�`@xbN@xQ�@x1'@w��@wK�@v�R@vV@v$�@u�@u@up�@u/@t��@t9X@st�@so@r��@r~�@r^5@r�@q�^@qX@p�`@p�u@pr�@pbN@pb@p  @o�;@o|�@oK�@o�@n�@n��@nv�@nV@n$�@n@m�@m@m�h@m/@l�@k33@jn�@i�^@i7L@h��@h1'@g�w@g\)@g�@f�R@fE�@f@e�-@e`B@eV@d�D@d�@c�
@cdZ@c@bn�@b-@bJ@aG�@`��@`bN@`1'@_��@_�@^��@]�@]�@]`B@]V@\�j@\Z@\1@[33@Z�\@Y�#@Y��@Yx�@Y7L@XĜ@XQ�@X  @W��@W|�@V�R@V5?@V$�@U�@U`B@UV@T��@T1@S��@S"�@R�H@R�!@R��@R�\@R�@Q�^@Q�7@QX@Q%@P��@P �@O�w@OK�@N�y@Nȴ@N�+@M�@Mp�@M�@L�@Lz�@LZ@L�@K�
@K�@KS�@K"�@Ko@J�H@Jn�@I��@I��@Ihs@I7L@I�@H��@HĜ@HQ�@G�;@G��@G��@G|�@G�@F��@F�@F��@F�+@Fff@FV@F5?@E�-@EO�@E?}@EV@D�/@D�@Dj@DI�@C��@C�m@C�
@C�F@C�F@CdZ@Co@B��@B^5@A�@AG�@@��@@�9@@Q�@@1'@@1'@@b@?��@?��@?�P@?;d@>��@>�R@>V@>@=@=�@=/@<��@<(�@;��@;ƨ@;�F@;t�@;o@:��@:n�@:=q@:�@9�#@9�7@97L@9&�@8�`@8Ĝ@8�9@8�@81'@7�;@7K�@7�@6ȴ@6�+@6v�@6E�@6@5�T@5��@5O�@5O�@5O�@5�@4�@49X@3��@3�
@3t�@333@3o@2�H@2��@2�\@2~�@2M�@1�#@1�7@1G�@0��@0�`@0Ĝ@0�u@0b@/�;@/�@/�P@/|�@/;d@.�y@.ȴ@.v�@.v�@.V@.5?@-�T@-�-@-O�@,�j@,�D@,z�@,9X@+��@+ƨ@+�F@+�@+o@*��@*�\@*^5@*J@)hs@)�@(��@(��@(�9@(r�@(Q�@(Q�@(Q�@(1'@(  @'�w@'|�@';d@&�R@&��@&v�@&V@&$�@%�T@%�h@%O�@%?}@$�/@$�j@$z�@$9X@#�m@#�F@#��@#dZ@#33@#33@#@"~�@"-@"-@"J@!�#@!x�@!X@!G�@!7L@ �`@ �u@ Q�@ b@��@�w@|�@K�@��@ȴ@�+@5?@��@�h@O�@�@�@j@I�@�m@��@dZ@��@�\@^5@�@��@�7@hs@hs@X@&�@��@r�@A�@1'@ �@b@�@�P@K�@��@�@�@�@ȴ@v�@E�@$�@��@��@�h@`B@?}@�@�@��@�@��@z�@I�@��@ƨ@��@t�@dZ@dZ@S�@S�@S�@S�@"�@�@��@�!@n�@�@�@��@�^@�^@�^@�^@��@hs@X@X@G�@7L@&�@��@��@��@Ĝ@Ĝ@�9@��@�u@�@r�@Q�@1'@b@ �@  @�w@�@�@|�@K�@K�@
=@�@ȴ@ȴ@�R@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��A�
=A��A��TA��
A���Aũ�Aş�Aŏ\Aŉ7A�hsA�G�A�=qA�1'A��A�{A�{A�bA�VA�bA�bA�JA�%A�%A�%A�%A�A���A���A���A��A��A��A��A��A��A��A��A��A��TA���AĬAě�A�t�A��AöFA��A���A¡�A�5?A���A���A�%A�G�A��A���A��A�VA��A��uA�A���A���A��!A�p�A�t�A��A�?}A���A��FA�&�A�dZA�A�K�A��A��-A�JA�jA���A��FA���A�9XA�A�|�A���A�VA�bNA���A���A�|�A�Q�A���A�hsA�=qA���A�E�A���A��A��A�XA�1'A��A��wA�-A��A�bNA��!A�M�A��/A�jA�9XA�bNA��jA�{A�t�A�
=A���A�7A{�wAz�Aw��Av��Av �As�wAq�AqhsAp��Ao��Am�Aj��Ail�Ah5?Af  AdZAbI�Aa��A_S�A]�wA\��A\E�A[��AY�ATI�AQ`BAP�DAOAM�TAM%AK�AI�AG�wAE�mAEG�ACx�AA�A?�
A=K�A;�A9�#A81'A7&�A5��A4 �A3S�A2�+A2JA1+A.��A.A-?}A,z�A+x�A*VA'|�A&A�A$�yA#�A"v�A!��A!p�A E�A�/A�7AZA�A �A`BAJAA�A�yA9XA;dA��AQ�A\)A�A�TA��A�AVA
�A
z�A
Q�A
{A	�PA��A�wA��A�\AAv�A33A��AI�AJA��A%A n�@��m@�9X@��9@�V@�hs@���@�h@���@�I�@�V@��@��@� �@�+@�$�@�D@�R@�;d@�v�@�O�@���@܋D@�~�@�bN@ם�@�ff@�X@�V@��/@Ԭ@�"�@��T@��@�S�@�ȴ@͙�@���@�9X@�ȴ@ə�@���@��@Ǯ@�o@ư!@���@�?}@���@ÍP@��#@�hs@��@��@�t�@��H@�~�@�$�@�`B@��9@�z�@�1'@���@�dZ@�o@���@�V@�-@�x�@��@���@��w@�;d@��@���@�~�@�-@��-@�O�@��@��`@��u@� �@��w@�+@���@���@�~�@�-@��@�@��7@�7L@���@���@�I�@���@��@��@�ȴ@���@�`B@��@���@�b@�ƨ@��P@�\)@�S�@�C�@�
=@�V@��@�G�@��@�1@��@��@��F@�\)@�C�@�
=@��y@��@��+@�=q@���@�%@���@���@�%@���@���@� �@�t�@�\)@��@�
=@�@���@�ȴ@�ȴ@���@���@�ff@�E�@�^5@�@�G�@�/@���@��`@��/@��j@��9@��@��u@��@��
@��F@�t�@�C�@��@��H@���@�n�@�5?@��@��@��@�x�@�O�@���@�Ĝ@��@���@�j@�A�@� �@���@��@�\)@��@��y@��+@�^5@�=q@�5?@�5?@��@���@�@���@�hs@���@��D@��@�r�@�Z@��
@��P@�t�@�;d@��H@��H@��@���@�@�@��@���@�~�@�^5@�M�@�5?@�@��7@�?}@�V@�%@��/@��9@���@�j@�I�@�(�@�  @��@�dZ@�S�@�C�@�33@���@���@���@�E�@�@��@�7L@���@��@��j@��u@�j@�b@�ƨ@�t�@�;d@���@�^5@���@�O�@�x�@�X@�%@��`@�Ĝ@���@��@��D@�I�@� �@��
@�dZ@�+@��H@�v�@�^5@�V@�V@�V@�V@�V@�J@��@��#@��h@�&�@���@�z�@�r�@�b@��w@��@�@��y@���@���@�M�@���@�O�@�%@���@��j@�z�@�I�@�  @��m@��w@���@�S�@�"�@���@�n�@�ff@�-@���@���@��7@�p�@�?}@�V@�%@��@���@���@��@��D@�A�@�;@\)@~��@}�@}`B@|��@|��@|Z@|I�@{�
@{ƨ@{��@{�@{S�@{33@z�H@zM�@y��@x�`@xbN@xQ�@x1'@w��@wK�@v�R@vV@v$�@u�@u@up�@u/@t��@t9X@st�@so@r��@r~�@r^5@r�@q�^@qX@p�`@p�u@pr�@pbN@pb@p  @o�;@o|�@oK�@o�@n�@n��@nv�@nV@n$�@n@m�@m@m�h@m/@l�@k33@jn�@i�^@i7L@h��@h1'@g�w@g\)@g�@f�R@fE�@f@e�-@e`B@eV@d�D@d�@c�
@cdZ@c@bn�@b-@bJ@aG�@`��@`bN@`1'@_��@_�@^��@]�@]�@]`B@]V@\�j@\Z@\1@[33@Z�\@Y�#@Y��@Yx�@Y7L@XĜ@XQ�@X  @W��@W|�@V�R@V5?@V$�@U�@U`B@UV@T��@T1@S��@S"�@R�H@R�!@R��@R�\@R�@Q�^@Q�7@QX@Q%@P��@P �@O�w@OK�@N�y@Nȴ@N�+@M�@Mp�@M�@L�@Lz�@LZ@L�@K�
@K�@KS�@K"�@Ko@J�H@Jn�@I��@I��@Ihs@I7L@I�@H��@HĜ@HQ�@G�;@G��@G��@G|�@G�@F��@F�@F��@F�+@Fff@FV@F5?@E�-@EO�@E?}@EV@D�/@D�@Dj@DI�@C��@C�m@C�
@C�F@C�F@CdZ@Co@B��@B^5@A�@AG�@@��@@�9@@Q�@@1'@@1'@@b@?��@?��@?�P@?;d@>��@>�R@>V@>@=@=�@=/@<��@<(�@;��@;ƨ@;�F@;t�@;o@:��@:n�@:=q@:�@9�#@9�7@97L@9&�@8�`@8Ĝ@8�9@8�@81'@7�;@7K�@7�@6ȴ@6�+@6v�@6E�@6@5�T@5��@5O�@5O�@5O�@5�@4�@49X@3��@3�
@3t�@333@3o@2�H@2��@2�\@2~�@2M�@1�#@1�7@1G�@0��@0�`@0Ĝ@0�u@0b@/�;@/�@/�P@/|�@/;d@.�y@.ȴ@.v�@.v�@.V@.5?@-�T@-�-@-O�@,�j@,�D@,z�@,9X@+��@+ƨ@+�F@+�@+o@*��@*�\@*^5@*J@)hs@)�@(��@(��@(�9@(r�@(Q�@(Q�@(Q�@(1'@(  @'�w@'|�@';d@&�R@&��@&v�@&V@&$�@%�T@%�h@%O�@%?}@$�/@$�j@$z�@$9X@#�m@#�F@#��@#dZ@#33@#33@#@"~�@"-@"-@"J@!�#@!x�@!X@!G�@!7L@ �`@ �u@ Q�@ b@��@�w@|�@K�@��@ȴ@�+@5?@��@�h@O�@�@�@j@I�@�m@��@dZ@��@�\@^5@�@��@�7@hs@hs@X@&�@��@r�@A�@1'@ �@b@�@�P@K�@��@�@�@�@ȴ@v�@E�@$�@��@��@�h@`B@?}@�@�@��@�@��@z�@I�@��@ƨ@��@t�@dZ@dZ@S�@S�@S�@S�@"�@�@��@�!@n�@�@�@��@�^@�^@�^@�^@��@hs@X@X@G�@7L@&�@��@��@��@Ĝ@Ĝ@�9@��@�u@�@r�@Q�@1'@b@ �@  @�w@�@�@|�@K�@K�@
=@�@ȴ@ȴ@�R@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�
B��B�B�
B�/B�5B�BB�HB�mB�B�B�B�B��B��B��BB%B%B+B1B1B1B1B
=B
=B
=B
=BDBJBPBPB\BbBbBhBhBbBbBbBbB\B\BuB�B�B�B$�B;dB:^B9XB?}BJ�BB�BD�BH�B,B0!B�B�B+B+BhB  BB��B�B��BB�B�B,B(�B$�B&�B/B'�B'�BPBB��B�B�5B�B�BB��BǮB�-B��Bu�B�{B�7B�B�B�Bt�Bk�B\)BO�BO�BN�BE�B/B
�B
�`B
��B
�3B
x�B
��B
��B
��B
��B
�B
e`B
R�B
VB
J�B
6FB
{B	��B	��B	�B	�yB	�sB	�/B	��B	�B	�fB	�B	�wB	�'B	�B	��B	�7B	�1B	v�B	|�B	jB	bNB	e`B	_;B	N�B	.B	B	B	{B	JB��B��B�fB�B�B��B��BȴB�^B�?B��B�'B��B��B�B��B��B��B��B��B�{B�B�1B�JB�B� Br�B`BBl�BjBm�BhsBn�Bp�BhsBcTBe`BffB_;BgmBgmB]/BT�BZB\)BT�BL�BO�BW
B^5BT�BO�BYB\)BbNBe`Be`BaHB]/BVBW
BYB_;BZBN�BR�B`BBe`Be`BaHB]/B^5BZBK�BO�BW
B_;BW
BP�BF�BK�BL�BS�BW
BXBQ�BQ�BM�BG�B?}BP�BR�BW
B]/BT�BR�B_;B]/B`BBffBgmBdZB]/B\)BcTBk�Br�Bq�Bw�Bz�Bw�B}�B�B�+B�JB�JB�bB�VB�hB�oB�VB�bB��B��B��B��B��B��B��B��B�B�'B�'B�'B�9B�?B�?B�XB�XB�LB�^B�qB�jBÖBǮBɺB��B��B��B��B��B��B��B��B�B�#B�BB�ZB�`B�ZB�mB�yB�yB�B�B�B�B�B��B��B��B	  B	B	DB	bB	hB	�B	�B	�B	 �B	 �B	�B	�B	#�B	#�B	'�B	/B	8RB	<jB	=qB	?}B	F�B	G�B	G�B	I�B	L�B	P�B	P�B	O�B	XB	YB	YB	[#B	^5B	\)B	[#B	bNB	e`B	hsB	jB	jB	m�B	n�B	n�B	n�B	p�B	p�B	s�B	p�B	t�B	z�B	z�B	~�B	� B	� B	�B	�B	�B	� B	�B	�1B	�DB	�VB	�\B	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�FB	�RB	�XB	�XB	�XB	�^B	�^B	�dB	�^B	�^B	�}B	��B	��B	��B	��B	ÖB	ŢB	ŢB	ǮB	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�)B	�5B	�BB	�;B	�BB	�HB	�BB	�HB	�NB	�HB	�BB	�ZB	�`B	�fB	�`B	�`B	�`B	�fB	�fB	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B	��B
B
B
B
%B
B
B
B
B
B
+B
1B
	7B
1B
	7B

=B
JB
JB
JB
JB
JB
DB
VB
bB
\B
VB
bB
hB
hB
hB
hB
uB
oB
uB
uB
oB
oB
hB
bB
hB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
"�B
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
"�B
"�B
!�B
�B
 �B
"�B
#�B
$�B
%�B
&�B
&�B
'�B
(�B
(�B
'�B
(�B
(�B
(�B
)�B
(�B
)�B
+B
+B
+B
,B
-B
-B
+B
,B
.B
/B
.B
-B
.B
/B
0!B
1'B
1'B
1'B
1'B
1'B
0!B
1'B
2-B
49B
5?B
49B
33B
49B
5?B
6FB
5?B
49B
5?B
7LB
6FB
5?B
6FB
6FB
6FB
7LB
7LB
9XB
:^B
:^B
:^B
9XB
9XB
;dB
;dB
:^B
;dB
:^B
<jB
;dB
<jB
=qB
=qB
<jB
=qB
>wB
>wB
?}B
@�B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
@�B
@�B
A�B
B�B
B�B
C�B
C�B
C�B
B�B
C�B
E�B
D�B
D�B
C�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
D�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
G�B
F�B
G�B
F�B
G�B
G�B
H�B
I�B
I�B
J�B
K�B
J�B
J�B
J�B
K�B
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
M�B
M�B
N�B
M�B
L�B
M�B
M�B
N�B
O�B
N�B
N�B
O�B
P�B
O�B
P�B
P�B
O�B
O�B
O�B
O�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
R�B
R�B
R�B
S�B
T�B
S�B
T�B
VB
VB
VB
VB
VB
VB
T�B
VB
W
B
W
B
XB
XB
W
B
VB
XB
XB
YB
YB
XB
XB
YB
YB
ZB
ZB
ZB
YB
ZB
YB
YB
[#B
\)B
[#B
[#B
\)B
\)B
\)B
[#B
\)B
\)B
]/B
\)B
\)B
]/B
^5B
^5B
_;B
^5B
_;B
`BB
`BB
_;B
_;B
_;B
_;B
_;B
^5B
`BB
`BB
aHB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
cTB
bNB
cTB
e`B
dZB
dZB
cTB
e`B
e`B
e`B
dZB
dZB
e`B
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
gmB
gmB
hsB
gmB
hsB
iyB
hsB
hsB
iyB
hsB
iyB
jB
iyB
jB
jB
jB
k�B
k�B
jB
iyB
jB
k�B
l�B
l�B
l�B
l�B
k�B
k�B
l�B
l�B
m�B
m�B
m�B
l�B
l�B
m�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
r�B
r�B
s�B
s�B
s�B
r�B
s�B
s�B
t�B
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
t�B
t�B
t�B
t�B
t�B
t�B
s�B
u�B
t�B
t�B
t�B
u�B
t�B
t�B
u�B
u�B
v�B
v�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�
B�2B�B�YB�dB�OB�vB�B�B�B�B��B��B�B�"B�HB9B?B%B+BKBKB1BKB
=B
XB
=B
XB^BJBPB�B\B}BbBhB�BbBbB�B�B�B�B�BB�B�B&fB;�B:�B:xB@�BK^BDBF?BI�B/5B2�B#:B��B1B�B�B�BmB�	B��B��B�B�	B�B-wB*�B&fB(XB0�B*KB)�B�B3B iB�iB�bB��B�BϫB��B��B�kBy�B�B��B��B��B��Bu�Bl�B]�BQ�BP�BOvBF�B1�B
��B
�*B
�gB
��B
}B
��B
�B
�B
��B
��B
iDB
V�B
W�B
L�B
9�B
eB
;B	�(B	��B	�6B	�B	��B	��B	�=B	�mB	�B	��B	��B	��B	��B	�B	�rB	y>B	~B	mwB	d@B	f�B	`BB	P�B	1�B	?B	MB	�B	�B�HB�`B�B��B�	B�&B�SB�B��B�8B�B�B��B�B��B��B��B��B��B��B�9B��B�lB��B��B��Bt�Bc�BncBlqBoBjKBo�Bq�Bj0BezBgRBh$BaHBh�Bh�B_;BWsB[�B]dBV�BN�BQ�BX_B^�BV�BQ�BZ7B]dBb�Be�Be�Ba�B^5BWsBX_BZkB_�B[WBQ BT�BaBe�Be�BbB^5B_VB[qBNpBR:BX�B`'BXyBR�BH�BM�BN<BT�BW�BX�BR�BR�BOBBI7BA�BQ�BS�BW�B]�BV�BTaB_�B^5B`�Bf�Bg�Bd�B^�B]dBd�Bl=BsMBr�Bx�B{�Bx�B~�B��B��B��B��B��B�(B�B��B�vB��B�/B�BB��B�NB�`B�>B�yB��B��B��B�vB��B��B��B��B��B��B�B��B��B�<B��B�B�#B�B�0B�JB�NB�FB�MB�MBՁBٚBۦB��B�tB�B�B�B��B�B��B��B��B��B�B�B�JB��B	 �B	�B	�B	�B	�B	�B	�B	B	 �B	!B	!B	dB	$&B	$tB	(�B	/�B	8lB	<�B	=�B	?�B	F�B	G�B	G�B	I�B	M6B	Q4B	QhB	P}B	X+B	Y1B	YKB	[WB	^�B	\�B	[�B	b�B	e�B	h�B	j�B	j�B	m�B	n�B	n�B	n�B	p�B	p�B	s�B	qAB	u%B	{B	{B	.B	�4B	�4B	�AB	� B	�UB	�iB	�mB	��B	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�&B	�*B	�0B	�QB	�QB	�]B	�iB	�vB	��B	�zB	�lB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�MB	�SB	�EB	�KB	�KB	�KB	�eB	ؓB	�xB	�jB	�\B	ߊB	�vB	�|B	��B	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	��B	��B	��B	��B	�B	��B	�	B	�B	�B	�2B	�+B	�$B	�*B	�DB	�"B	�B	�B	�B	��B	�(B	�B	�"B	�(B	�PB	�JB	�jB
;B
;G�O�B
oB
aB
uB
YB
SB
gB
�B
�B
mB
zB
�B
	lB
�B
	�B

�B
~B
~B
�B
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
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
B
�B
�B
�B
 �B
�B
�B
 B
!B
 �B
!�B
!�B
 �B
"�B
!�B
!�B
"�B
#B
#B
"�B
$B
$B
$B
$B
#�B
"�B
"�B
"B
 BB
!B
#:B
$@B
%,B
&2B
'8B
'8B
($B
)*B
)*B
($B
)*B
)*B
)DB
*0B
)DB
*KB
+6B
+QB
+QB
,WB
-)B
-]G�O�B
,WB
.IB
/OB
.cB
-]B
.cB
/iB
0oB
1[B
1[B
1[B
1[B
1vB
0�B
1vB
2|B
4nB
5tB
4nB
3�B
4�B
5ZB
6zB
5�B
4�B
5�B
7fB
6zB
5tB
6zB
6zB
6�B
7�B
7�B
9�B
:�B
:�B
:�B
9�B
9�B
;�B
;�B
:�B
;�B
:�B
<�B
;�B
<�B
=�B
=�B
<�B
=�B
>�B
>�B
?�B
@�B
?�B
@�B
@�B
A�B
A�B
A�B
A�B
@�B
@�B
A�B
B�B
B�B
C�B
C�B
C�B
B�B
C�B
E�B
D�B
D�B
C�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
D�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
G�B
F�B
G�B
F�B
G�B
G�B
H�B
I�B
I�B
J�B
K�B
J�B
J�B
J�B
K�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
LB
K�B
NB
NB
OB
NB
MB
NB
M�B
OB
O�B
OB
OB
PB
Q B
PB
Q B
QB
O�B
O�B
P.B
P.B
R B
R B
R B
S&B
S&B
S&B
S&B
S&B
S&B
TB
TB
S&B
S@B
S@B
T,B
UB
TFB
U2B
VB
V9B
V9B
V9B
V9B
VB
UMB
V9B
W?B
W?B
X+B
X+B
W?B
VSB
X+B
X+B
YKB
Y1B
XEB
XEB
Y1B
YKB
Z7B
ZQB
ZQB
Y1B
ZQB
YeB
YeB
[WB
\]B
[WB
[WB
\]B
\]B
\]B
[qB
\]B
\CB
]dB
\]B
\xB
]dB
^jB
^OB
_VB
^jB
_VB
`\B
`BB
_VB
_pB
_pB
_VB
_pB
^�B
`\B
`vB
a|B
`\B
`\B
a|B
abB
a|B
a|B
b�B
abB
b�B
b�B
b�B
c�B
c�B
cnB
dtB
c�B
b�B
c�B
ezB
d�B
dtB
c�B
e`B
ezB
e�B
d�B
d�B
e�B
e�B
ezB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
h�B
g�B
h�B
i�B
h�B
h�B
i�B
h�B
i�B
j�B
i�B
j�B
j�B
j�B
k�B
k�B
j�B
i�B
j�B
k�B
l�B
l�B
l�B
l�B
k�B
k�B
l�B
l�B
m�B
m�B
m�B
l�B
l�B
m�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
r�B
r�B
s�B
s�B
s�B
r�B
s�B
s�B
t�B
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
t�B
t�B
t�B
t�B
t�B
t�B
s�B
u�B
t�B
t�B
t�B
u�B
t�B
t�B
u�B
u�B
v�B
v�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.17(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201801240036002018012400360020180124003600201806221325082018062213250820180622132508201804050728322018040507283220180405072832  JA  ARFMdecpA19c                                                                20180120093516  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180120003524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180120003526  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180120003527  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180120003527  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180120003527  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180120003527  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20180120003527  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20180120003527  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180120003527  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20180120003528  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180120003528                      G�O�G�O�G�O�                JA  ARUP                                                                        20180120005449                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180120153508  CV  JULD            G�O�G�O�F�/�                JM  ARSQJMQC2.0                                                                 20180121000000  CF  TEMP_ADJUSTED_QCCP  CR  G�O�                JM  ARSQJMQC2.0                                                                 20180122000000  CF  PSAL_ADJUSTED_QCCP  D�  G�O�                JM  ARCAJMQC2.0                                                                 20180123153600  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180123153600  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222832  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042508  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221516                      G�O�G�O�G�O�                