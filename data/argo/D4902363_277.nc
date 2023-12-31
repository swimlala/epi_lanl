CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-09-05T00:35:15Z creation;2018-09-05T00:35:19Z conversion to V3.1;2019-12-19T07:33:29Z update;     
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
_FillValue                  �  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20180905003515  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0576_277                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�~���݀1   @�~�q��@9�ݗ�+k�d\�s�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bo��Bx  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D � D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+y�D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DLfDL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw�fDx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׃3D�� D�  D�C3D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@��\@��\AG�A=G�A]G�A}G�A���A���A���A���AΣ�Aޣ�A��A���BQ�BQ�BQ�BQ�B'Q�B/Q�B7Q�B?Q�BGQ�BOQ�BWQ�B_Q�BgQ�Bn�BwQ�BQ�B��)B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bè�BǨ�B˨�BϨ�BӨ�Bר�Bۨ�Bߨ�B��B��B��B��B��B���B���B���C�{C�{C�{C�{C	�{C�{C�{C�{C�{C�{C�{C�{C�{C�C�{C�{C!�{C#�{C%�{C'�{C)�{C+�{C-�{C/�{C1�{C3�{C5�{C7�{C9�{C;�{C=�{C?�{CA�{CC�{CE�{CG�{CI�{CK�{CM�{CO�{CQ�{CS�{CU�{CW�{CY�{C[�{C]�{C_�{Ca�{Cc�{Ce�{Cg�{Ci�{Ck�{Cm�{Co�{Cq�{Cs�{Cu�{Cw�{Cy�{C{�{C}�{C�{C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��qC��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��
C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��
C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��qD uD �DuD�DuD�DuD��DuD�DuD�DuD�DuD�DuD�D	uD	�D
uD
�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D{�D�DuD�DuD�D uD �D!uD!�D"uD"�D#uD#�D$uD$�D%uD%�D&uD&�D'uD'�D(uD(�D)uD)�D*uD*�D+n�D+�D,uD,�D-uD-�D.uD.�D/uD/�D0uD0�D1uD1�D2uD2�D3uD3�D4uD4�D5uD5�D6uD6�D7uD7�D8uD8�D9uD9�D:uD:�D;uD;�D<uD<�D=uD=�D>uD>�D?uD?�D@uD@�DAuDA�DBuDB�DCuDC�DDuDD�DEuDE�DFuDF�DGuDG�DHuDH�DIuDI�DJuDJ�DKuDK��DLuDL�DMuDM�DNuDN�DOuDO�DPuDP�DQuDQ�DRuDR�DSuDS�DTuDT�DUuDU�DVuDV�DWuDW�DXuDX�DYuDY�DZuDZ�D[uD[�D\uD\�D]uD]�D^uD^�D_uD_�D`uD`�DauDa�DbuDb�DcuDc�DduDd�DeuDe�DfuDf�DguDg�DhuDh�DiuDi�DjuDj�DkuDk�DluDl�DmuDm�DnuDn�DouDo�DpuDp�DquDq�DruDr�DsuDs�DtuDt�DuuDu�DvuDv�Dw{�Dw�DxuDx�DyuDy�DzuDz�D{uD{�D|uD|�D}uD}�D~uD~�DuD�D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�}�D׺�D���D�=�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�DຏD���D�:�D�z�DẏD���D�:�D�z�D⺏D���D�:�D�z�D㺏D���D�:�D�z�D亏D���D�:�D�z�D序D���D�:�D�z�D溏D���D�:�D�z�D纏D���D�:�D�z�D躏D���D�:�D�z�D麏D���D�:�D�z�D꺏D���D�7\D�z�D뺏D���D�:�D�z�D캏D���D�:�D�z�D���D���D�:�D�z�DD���D�:�D�z�DﺏD���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�=�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A��HA��;A��;A��/A��A��A��A���A��
A��A��A��#A��#A��#A��/A��#A��A��A��A��#A��/A��/A��AҸRA�ĜAϥ�A���A�?}Aŏ\A�7LA�K�A�1A��TA�XA�x�A��
A�E�A�  A��`A�
=A�9XA��yA��RA�O�A�VA�p�A�{A�9XA��#A�%A��^A�oA�JA��;A�dZA��A��7A�A���A�M�A�{A��-A��A��\A�O�A���A�%A�ZA� �A���A�`BA���A�JA���A��PA� �A���A�33A�r�A���A��FA�33A���A�  A��-A���A�5?A���A�A�jA��
A�M�A���A�  A��A���A�jA���A�M�A��!A��yA� �A�A�XA���A�wA|�RA|bA{��A{G�A{%AzE�Ayx�AyoAw�wAv��Av��Av�jAvVAu7LAt��Atn�AshsAr��Ar��AqAoG�Al��Ak��AkAk��Aj��Ajn�Ai33Ah^5Ag�hAg33Af�!AeK�Ab �A`ȴA_�;A^v�A\�A\M�A[|�AZ�AYG�AW�TAW7LAV�+AU�^AS�AR �AP��AO�AN�+AL^5AK�AJĜAI|�AHJAEO�ADbNAC&�AA�wAAVA?&�A>A�A=��A=l�A;��A:��A9x�A9
=A8r�A7�^A6�A6A4M�A3XA2�`A2�9A2�DA1hsA0M�A/�hA.ȴA-��A,M�A+��A+
=A)\)A(�+A'��A'��A'�A&�yA&�A%�;A%A$JA#�wA#A"{A!t�A v�A�A�9AjA�A�;A\)A�A^5A�hA�/A��A�A �A/A{AoA��A�AA��A�7A%A
��A
ĜA
M�A
A	�;A	��A	��A�;A�\A��AA��Ar�AA�hAG�A�9A��A�A E�@�-@�%@��P@�
=@��\@��^@���@���@�@�F@�@��@��D@�u@�1'@�C�@�p�@�@�R@�J@��@�~�@�l�@�`B@��@�b@�@�S�@ج@�z�@�Q�@��@���@׾w@ם�@�dZ@���@֗�@��T@�I�@�-@��@мj@�(�@ΰ!@���@��/@˶F@�ȴ@�1@�ȴ@���@�Q�@�1'@���@�\)@�
=@�@�bN@�Q�@�I�@��@�C�@���@�7L@��j@�r�@��w@�ȴ@�v�@�$�@���@�O�@��@�A�@��@��m@��T@� �@��\@�Z@�dZ@��@���@�V@�hs@��D@�@��@���@�ff@���@��@��@�=q@�`B@�b@�;d@��@���@�=q@��^@��j@�r�@� �@�\)@���@���@��!@���@�?}@��/@��@��@��@���@���@�n�@�{@��^@���@�p�@�&�@��D@�bN@�9X@���@�;d@��H@�~�@�{@��#@���@���@�`B@���@��j@�Z@���@���@�t�@�t�@�l�@�\)@�S�@�C�@�o@���@�M�@���@��@�9X@��w@���@�ȴ@��\@��@�@���@���@���@���@���@���@��7@�&�@�z�@��
@���@�|�@�+@��+@�=q@��@���@�x�@�V@��@���@��@��u@�r�@�j@�Q�@��@��w@���@��@�dZ@�;d@�ȴ@���@��+@�v�@�=q@��@��@�x�@��/@�r�@�(�@�1@�  @�w@~�R@}�T@}�-@}/@|�j@{��@{dZ@z�@z��@z�!@z�!@z�!@z�!@z~�@y�7@x��@xb@w��@wK�@w
=@vv�@u��@u�-@u��@u`B@t��@tZ@t(�@s�
@st�@sC�@r^5@q�7@qG�@q%@p��@p1'@o�;@o�P@oK�@n��@nE�@nE�@nv�@nff@n��@n��@nff@n5?@n@n@nE�@nE�@nV@n@l9X@j��@j��@j�!@j^5@j~�@j=q@j�@ix�@h1'@hb@h  @g�;@gl�@fȴ@f{@e�T@e��@e�h@ep�@e`B@e?}@e/@eV@d�@dZ@c�m@cdZ@co@b~�@a�@a�^@ax�@a7L@`��@`Q�@`b@`  @_�@_�P@^��@^V@^$�@]�T@]��@]O�@\�@\��@\j@\I�@\1@[��@[o@Z��@Z�\@Z=q@Z-@Y�@Yhs@Y7L@Y�@X��@X��@X��@XbN@X1'@W�w@V�R@VV@V@UO�@U�@T�/@Tz�@S��@S��@S�@St�@S33@S"�@R�H@R�@Q�@Q�#@Q��@Qhs@QX@Q&�@Q%@P��@PbN@P  @O��@O�@N��@Nff@NE�@N$�@M��@Mp�@L�j@L�D@Lz�@Lj@LZ@L9X@L1@Kƨ@K��@K�@KdZ@K@J��@Jn�@I�#@IX@H��@H�@HbN@HA�@H �@G�@G��@G��@G\)@G�@G
=@F�@Fv�@F$�@E�-@E�@E`B@D��@D1@C��@CdZ@B�H@Bn�@B^5@BM�@BM�@B=q@A��@AG�@@��@@Ĝ@@��@@�@@  @?��@?\)@?
=@>ȴ@>�R@>v�@>@=@=��@=�@=?}@=V@<�@<I�@<1@;�m@;ƨ@;�@;C�@:�@:�\@:J@9��@9x�@9�@8��@8Ĝ@8�u@8bN@8A�@8b@8  @7��@7|�@7;d@7
=@7
=@6v�@5�T@5�-@5��@5O�@4�@4��@4z�@3�m@3�F@3��@3��@333@3"�@2�!@2^5@2M�@2�@1��@1x�@1G�@1�@0�9@0�@0A�@0 �@0b@0  @/�@/��@/�w@/�P@/K�@.�y@.ȴ@.�R@.��@.��@.�+@.V@.@-��@-�-@-��@-�@-`B@,��@,�@,�@,��@,�D@,Z@,1@+��@*�@*�H@*��@*��@*~�@*n�@*^5@*-@)��@)�@)�#@)��@)��@)&�@)%@)�@(�`@(�@(r�@(r�@(r�@'��@'�@'\)@'K�@'K�@'K�@'+@&��@&�@&��@&V@&$�@%�@%�-@%�h@%p�@%O�@$��@$�j@$�@#�m@"�H@"~�@"n�@"-@"�@!x�@ Ĝ@ �u@ A�@�w@K�@+@
=@�@�+@{@�-@/@��@��@z�@�@�F@C�@@�H@��@�\@�@�@��@�7@x�@X@��@�9@��@�@A�@�@�@l�@��@ȴ@�R@V@$�@�@��@�-@�@O�@?}@�j@z�@(�@1@ƨ@t�@S�@S�@S�@33@o@�@��@��@^5@�@�#@x�@G�@7L@�`@r�@Q�@1'@b@��@�@;d@
=@�R@v�@ff@E�@{@�-@O�@/@/@�@V@V@�@�@V@��@�@9X@��@�F@�@t�@t�@dZ@S�@C�@"�@@
�H@
�\@
�\@
~�@	��@	��@	�^@	�^@	x�@	G�@	7L@	%@��@�u@�@A�@  @�@�@��@|�@l�@\)@��@ȴ@�R@�R@��@��@��@��@v�@$�@�@�@`B@O�@?}@/@��@��@�@j@Z@I�@I�@(�@��@�m@ƨ@��@dZ@33@"�@"�@"�@o@�H@�\@n�@n�@^5@-@�@�^@�^@��@��@X@&�@�@ ��@ ��@ Ĝ@ �9@ �u@ �@ r�@ bN@ A�@  �?��w?�\)?��?���?��R?��R?���?�5??��?���?��-?��-?��h?��h?�p�?�p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A��HA��;A��;A��/A��A��A��A���A��
A��A��A��#A��#A��#A��/A��#A��A��A��A��#A��/A��/A��AҸRA�ĜAϥ�A���A�?}Aŏ\A�7LA�K�A�1A��TA�XA�x�A��
A�E�A�  A��`A�
=A�9XA��yA��RA�O�A�VA�p�A�{A�9XA��#A�%A��^A�oA�JA��;A�dZA��A��7A�A���A�M�A�{A��-A��A��\A�O�A���A�%A�ZA� �A���A�`BA���A�JA���A��PA� �A���A�33A�r�A���A��FA�33A���A�  A��-A���A�5?A���A�A�jA��
A�M�A���A�  A��A���A�jA���A�M�A��!A��yA� �A�A�XA���A�wA|�RA|bA{��A{G�A{%AzE�Ayx�AyoAw�wAv��Av��Av�jAvVAu7LAt��Atn�AshsAr��Ar��AqAoG�Al��Ak��AkAk��Aj��Ajn�Ai33Ah^5Ag�hAg33Af�!AeK�Ab �A`ȴA_�;A^v�A\�A\M�A[|�AZ�AYG�AW�TAW7LAV�+AU�^AS�AR �AP��AO�AN�+AL^5AK�AJĜAI|�AHJAEO�ADbNAC&�AA�wAAVA?&�A>A�A=��A=l�A;��A:��A9x�A9
=A8r�A7�^A6�A6A4M�A3XA2�`A2�9A2�DA1hsA0M�A/�hA.ȴA-��A,M�A+��A+
=A)\)A(�+A'��A'��A'�A&�yA&�A%�;A%A$JA#�wA#A"{A!t�A v�A�A�9AjA�A�;A\)A�A^5A�hA�/A��A�A �A/A{AoA��A�AA��A�7A%A
��A
ĜA
M�A
A	�;A	��A	��A�;A�\A��AA��Ar�AA�hAG�A�9A��A�A E�@�-@�%@��P@�
=@��\@��^@���@���@�@�F@�@��@��D@�u@�1'@�C�@�p�@�@�R@�J@��@�~�@�l�@�`B@��@�b@�@�S�@ج@�z�@�Q�@��@���@׾w@ם�@�dZ@���@֗�@��T@�I�@�-@��@мj@�(�@ΰ!@���@��/@˶F@�ȴ@�1@�ȴ@���@�Q�@�1'@���@�\)@�
=@�@�bN@�Q�@�I�@��@�C�@���@�7L@��j@�r�@��w@�ȴ@�v�@�$�@���@�O�@��@�A�@��@��m@��T@� �@��\@�Z@�dZ@��@���@�V@�hs@��D@�@��@���@�ff@���@��@��@�=q@�`B@�b@�;d@��@���@�=q@��^@��j@�r�@� �@�\)@���@���@��!@���@�?}@��/@��@��@��@���@���@�n�@�{@��^@���@�p�@�&�@��D@�bN@�9X@���@�;d@��H@�~�@�{@��#@���@���@�`B@���@��j@�Z@���@���@�t�@�t�@�l�@�\)@�S�@�C�@�o@���@�M�@���@��@�9X@��w@���@�ȴ@��\@��@�@���@���@���@���@���@���@��7@�&�@�z�@��
@���@�|�@�+@��+@�=q@��@���@�x�@�V@��@���@��@��u@�r�@�j@�Q�@��@��w@���@��@�dZ@�;d@�ȴ@���@��+@�v�@�=q@��@��@�x�@��/@�r�@�(�@�1@�  @�w@~�R@}�T@}�-@}/@|�j@{��@{dZ@z�@z��@z�!@z�!@z�!@z�!@z~�@y�7@x��@xb@w��@wK�@w
=@vv�@u��@u�-@u��@u`B@t��@tZ@t(�@s�
@st�@sC�@r^5@q�7@qG�@q%@p��@p1'@o�;@o�P@oK�@n��@nE�@nE�@nv�@nff@n��@n��@nff@n5?@n@n@nE�@nE�@nV@n@l9X@j��@j��@j�!@j^5@j~�@j=q@j�@ix�@h1'@hb@h  @g�;@gl�@fȴ@f{@e�T@e��@e�h@ep�@e`B@e?}@e/@eV@d�@dZ@c�m@cdZ@co@b~�@a�@a�^@ax�@a7L@`��@`Q�@`b@`  @_�@_�P@^��@^V@^$�@]�T@]��@]O�@\�@\��@\j@\I�@\1@[��@[o@Z��@Z�\@Z=q@Z-@Y�@Yhs@Y7L@Y�@X��@X��@X��@XbN@X1'@W�w@V�R@VV@V@UO�@U�@T�/@Tz�@S��@S��@S�@St�@S33@S"�@R�H@R�@Q�@Q�#@Q��@Qhs@QX@Q&�@Q%@P��@PbN@P  @O��@O�@N��@Nff@NE�@N$�@M��@Mp�@L�j@L�D@Lz�@Lj@LZ@L9X@L1@Kƨ@K��@K�@KdZ@K@J��@Jn�@I�#@IX@H��@H�@HbN@HA�@H �@G�@G��@G��@G\)@G�@G
=@F�@Fv�@F$�@E�-@E�@E`B@D��@D1@C��@CdZ@B�H@Bn�@B^5@BM�@BM�@B=q@A��@AG�@@��@@Ĝ@@��@@�@@  @?��@?\)@?
=@>ȴ@>�R@>v�@>@=@=��@=�@=?}@=V@<�@<I�@<1@;�m@;ƨ@;�@;C�@:�@:�\@:J@9��@9x�@9�@8��@8Ĝ@8�u@8bN@8A�@8b@8  @7��@7|�@7;d@7
=@7
=@6v�@5�T@5�-@5��@5O�@4�@4��@4z�@3�m@3�F@3��@3��@333@3"�@2�!@2^5@2M�@2�@1��@1x�@1G�@1�@0�9@0�@0A�@0 �@0b@0  @/�@/��@/�w@/�P@/K�@.�y@.ȴ@.�R@.��@.��@.�+@.V@.@-��@-�-@-��@-�@-`B@,��@,�@,�@,��@,�D@,Z@,1@+��@*�@*�H@*��@*��@*~�@*n�@*^5@*-@)��@)�@)�#@)��@)��@)&�@)%@)�@(�`@(�@(r�@(r�@(r�@'��@'�@'\)@'K�@'K�@'K�@'+@&��@&�@&��@&V@&$�@%�@%�-@%�h@%p�@%O�@$��@$�j@$�@#�m@"�H@"~�@"n�@"-@"�@!x�@ Ĝ@ �u@ A�@�w@K�@+@
=@�@�+@{@�-@/@��@��@z�@�@�F@C�@@�H@��@�\@�@�@��@�7@x�@X@��@�9@��@�@A�@�@�@l�@��@ȴ@�R@V@$�@�@��@�-@�@O�@?}@�j@z�@(�@1@ƨ@t�@S�@S�@S�@33@o@�@��@��@^5@�@�#@x�@G�@7L@�`@r�@Q�@1'@b@��@�@;d@
=@�R@v�@ff@E�@{@�-@O�@/@/@�@V@V@�@�@V@��@�@9X@��@�F@�@t�@t�@dZ@S�@C�@"�@@
�H@
�\@
�\@
~�@	��@	��@	�^@	�^@	x�@	G�@	7L@	%@��@�u@�@A�@  @�@�@��@|�@l�@\)@��@ȴ@�R@�R@��@��@��@��@v�@$�@�@�@`B@O�@?}@/@��@��@�@j@Z@I�@I�@(�@��@�m@ƨ@��@dZ@33@"�@"�@"�@o@�H@�\@n�@n�@^5@-@�@�^@�^@��@��@X@&�@�@ ��@ ��@ Ĝ@ �9@ �u@ �@ r�@ bN@ A�@  �?��w?�\)?��?���?��R?��R?���?�5??��?���?��-?��-?��h?��h?�p�?�p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�%B�%B�%B�%B�%B�%B�+B�+B�+B�+B�%B�+B�+B�%B�%B�+B�+B�+B�%B�B�Bz�Bk�B[#B^5BI�B=qBVBM�BS�B~�Bz�BiyBt�B��Bv�BbNBL�B�VB�B�'B��B�PBw�BF�Bm�BVBXBZB;dB49B2-B.B(�BF�BC�BI�BH�BG�B>wB49B-B�B�BoB�ZB�fB�B�B�mB�mB�B�B�BB��B�}B��B��B�\B��B��B�1BdZBm�B_;BaHBS�B@�B�B
��BB
�B
��B
ǮB
�LB
�3B
�?B
��B
�=B
jB
\)B
l�B
`BB
N�B
33B
S�B
W
B
T�B
P�B
G�B
<jB
>wB
0!B
1'B
?}B
@�B
9XB
)�B
2-B
1'B
"�B
#�B
�B
B	��B	�B	��B
+B
B	��B	�B	�mB	�NB	�BB	�BB	��B	�jB	��B	�B	��B	��B	�{B	��B	�{B	�B	�B	w�B	y�B	o�B	aHB	K�B	?}B	@�B	5?B	/B	�B	0!B	 �B	hB	  B�B��B��B�B�B�5B�ZB�B�B�/B�yB�mB�B�B�TB�#B�
B�wBǮB��B��BȴB�9B�B�B��B��B�\B��B�{B�%B�VB��B��B��B�oB�\B�DB�B~�B�%B|�Br�Br�BffB]/B;dB<jBF�BZB]/B\)BVBO�BK�BF�BJ�BD�B@�B=qB<jBG�BE�BB�B:^B2-BB�BI�BF�B@�BA�BC�B?}B5?B�B�B+B0!B;dB33B,B6FB1'B'�B#�B�B�B�B#�B$�B+B)�B!�B�B{B�B!�B�B'�B)�B-B%�B�B{B{B�B�B{BBBDB�B�B\B��B	7B'�B(�B(�B(�B'�B&�B$�B!�B�B�BVBJB�B!�B�B�B�B�B�B�B%B�B�B(�B0!B,B(�B'�B�B�B0!B.B)�B#�B$�B�B,B-B+B)�B33B49B33B2-B2-B5?B6FB0!B �B#�B.B-BB�BI�BL�BH�BB�BB�B<jB>wBG�BN�BS�BcTB`BBW
Be`BffBr�B|�B}�B|�B{�By�B�B�B�B�1B�JB�PB�1B�DB�oB��B��B��B��B��B��B��B��B�B�B�B�B�9B�?B�?B�3B�^B�jB�}BÖBŢBĜBÖBĜBƨBǮBɺB��B��B��B��B��B��B��B��B��B��B��B��B�
B�)B�#B�fB�`B�ZB�B�B�B�B�B�B�B�B�yB�yB�B�B��B��B��B��B	B	B		7B	+B	PB	VB	bB	hB	oB	uB	uB	uB	{B	�B	�B	�B	�B	�B	$�B	(�B	)�B	)�B	+B	+B	)�B	,B	5?B	9XB	>wB	?}B	>wB	;dB	@�B	F�B	F�B	H�B	K�B	P�B	T�B	ZB	^5B	`BB	aHB	aHB	`BB	_;B	dZB	ffB	l�B	n�B	o�B	n�B	q�B	v�B	x�B	w�B	v�B	|�B	}�B	}�B	}�B	� B	� B	�B	�DB	�JB	�VB	�bB	�hB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	�B	�'B	�'B	�3B	�FB	�RB	�FB	�?B	�qB	�wB	�qB	�jB	�dB	�jB	��B	ÖB	ÖB	ĜB	ŢB	ŢB	ŢB	ĜB	ĜB	ƨB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	��B	��B	�B	�#B	�)B	�)B	�)B	�/B	�5B	�BB	�BB	�BB	�;B	�HB	�TB	�`B	�`B	�mB	�fB	�fB	�yB	�B	�B	�B	�B	�B	�B	�yB	�sB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B	��B	��B	��B
B
B
B
B
B
B
B
%B
%B
%B
B
+B
%B
%B
+B
	7B
DB
JB
PB
PB
PB
PB
PB
PB
PB
VB
VB
PB
VB
VB
bB
bB
VB
VB
oB
{B
uB
uB
�B
�B
�B
�B
{B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
$�B
$�B
%�B
%�B
&�B
%�B
&�B
&�B
%�B
%�B
&�B
'�B
%�B
%�B
(�B
)�B
(�B
(�B
+B
)�B
)�B
-B
.B
-B
,B
-B
,B
-B
/B
.B
.B
.B
/B
0!B
/B
0!B
1'B
2-B
33B
33B
2-B
2-B
33B
2-B
2-B
2-B
49B
5?B
5?B
5?B
5?B
5?B
49B
5?B
6FB
7LB
6FB
7LB
7LB
9XB
9XB
9XB
7LB
8RB
8RB
7LB
8RB
;dB
<jB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
@�B
?}B
?}B
A�B
A�B
@�B
>wB
A�B
A�B
C�B
D�B
D�B
C�B
C�B
C�B
B�B
B�B
C�B
D�B
C�B
D�B
D�B
D�B
D�B
C�B
D�B
A�B
?}B
C�B
E�B
D�B
D�B
B�B
B�B
F�B
E�B
F�B
F�B
I�B
I�B
I�B
H�B
H�B
I�B
J�B
K�B
L�B
K�B
L�B
L�B
M�B
N�B
P�B
P�B
O�B
N�B
P�B
P�B
Q�B
R�B
Q�B
P�B
Q�B
R�B
R�B
Q�B
Q�B
R�B
S�B
R�B
T�B
VB
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
W
B
VB
W
B
XB
YB
YB
YB
ZB
\)B
[#B
[#B
[#B
[#B
ZB
[#B
ZB
ZB
[#B
[#B
\)B
]/B
\)B
\)B
^5B
_;B
^5B
^5B
^5B
]/B
_;B
_;B
`BB
aHB
aHB
`BB
_;B
`BB
cTB
dZB
cTB
dZB
dZB
dZB
dZB
cTB
bNB
aHB
aHB
cTB
dZB
dZB
ffB
ffB
ffB
ffB
ffB
e`B
e`B
e`B
dZB
ffB
ffB
dZB
ffB
gmB
gmB
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
jB
jB
jB
jB
jB
iyB
jB
l�B
l�B
l�B
l�B
l�B
k�B
jB
jB
k�B
jB
m�B
m�B
n�B
m�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
o�B
p�B
o�B
p�B
q�B
r�B
r�B
r�B
q�B
q�B
r�B
s�B
r�B
r�B
r�B
r�B
t�B
t�B
s�B
s�B
s�B
t�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
t�B
t�B
t�B
u�B
v�B
v�B
v�B
w�B
v�B
v�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�MB�9B�B�%B�?B�?B�%B�?B�%B�EB�EB�EB�+B�%B�EB�+B�%B�%B�+B�EB�+B�%B�9B�uB|Bn�B`�Be�BS�B<�BV�BP.BVSB�B|�Bm�Bx8B��B|Bg�BTB��B��B��B�fB�.B|BLBo5BX�BZB[qB?.B6�B5%B1AB+�BG�BD�BJ�BIlBHKB?�B5�B.�ByB?B,B��B��B�B�B�*B��B�B�WB�HB��B�B��B�|B��B��B��B�=Bg�BoOBa-Bb�BU�BB�BjB
�cB�B
��B
�"B
�	B
�B
��B
�`B
��B
��B
n�B
^�B
nIB
a�B
QNB
6�B
T�B
W�B
U�B
Q�B
H�B
=qB
?HB
1�B
2-B
?�B
@�B
:B
+�B
2�B
1�B
$&B
$�B
�B
_B	�*B	�;B	��B
zB
�B	��B	�B	��B	�TB	�-B	��B	�B	��B	�B	��B	�_B	�vB	�SB	��B	��B	��B	�SB	yrB	z�B	p�B	b�B	NpB	AoB	B'B	6�B	0�B	�B	1B	"NB	@B	[B�B�DB��B�iB��B��B�B�IB�B�VB��B��B�UB�B�B�xB�EB��B��B�vB�\B�7B��B�}B�/B�>B�B��B�~B��B�KB��B�mB��B��B�uB�.B�JB�{B�4B��B~(BtBs�Bh
B_!B?}B?cBIB[	B^B\�BW
BQBMBHfBK�BFBBB?B=�BHfBFtBC{B;�B4BC-BJ	BGBAUBBBC�B@ B6B!BWB,WB1B;�B4B-B6�B1�B)B%B�B �B=B$�B%�B+�B*B"�BB�B �B"�BB(sB*eB-CB&�B�B�B�BjB~B�BBB�B7B?B}B�jB
�B(
B)DB)DB)DB($B'8B%FB"NB;ByB�B�BWB"NBpB�BqBxB�B�BKB�B�B)�B0oB,�B)�B(�B�B�B0!B.cB*B$�B%zB �B,�B-�B+�B*�B3�B4�B3�B2�B2�B5�B6�B0�B"�B%`B/iB.�BC-BJ	BM6BIRBC{BC{B=�B?�BH�BO�BT�Bc�B`�BXyBe�Bg�BshB}<B~]B}qB|jBz�B�aB��B��B��B��B��B��B��B��B��B�
B��B�	B�=B�BB�XB�eB�iB�iB�}B��B�nB�tB��B��B��B��B��B��B��B��B��B�B�B�B�#B�6B� B�&B�&B�&B�&B�4B�.B�VB�HB�vB͹B׍BܬB��B�B��B��B��B��B�B��B��B��B��B��B��B�0B� B��B�B�+B�`B�<B	[B	�B		lB	�B	jB	pB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	5B	%,B	)*B	*B	*KB	+QB	+kB	*B	,�B	5�B	9�B	>�B	?�B	>�B	;�B	@�B	F�B	GB	H�B	L0B	Q4B	UMB	ZQB	^OB	`BB	aHB	a|B	`�B	_�B	d�B	f�B	l�B	n�B	o�B	o B	q�B	v�B	y	B	xB	w2B	}"B	~(B	~BB	~BB	�OB	��B	��B	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�6B	�=B	�_B	��B	�hB	�B	�AB	�AB	�MB	�zB	�lB	��B	��B	�qB	��B	��B	��B	��B	��B	��B	��B	ðB	��B	żB	��B	��B	ĶB	��B	��B	��B	��B	�B	�B	�B	� B	� B	� B	�@B	�@B	�9B	�?B	�?B	�2B	�gB	�QB	�=B	�]B	�]B	�]B	�dB	�jB	�vB	�vB	�vB	ߊB	�B	�nB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	� B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�*B	�0B	�0B	�6B	�<B
 4B
;B
 4B	�HB	�HB	�cB
3B
9B
SB
SB
MB
MB
SB
YB
YB
YB
SB
_B
tB
YB
zB
	�B
xB
~B
�B
�B
�B
�B
�B
�B
�B
�B
pB
�B
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
%B
%B
&B
&B
'B
&B
'B
'B
&B
%�B
'B
(
B
&2B
&2B
)*B
*B
)*B
)*B
+6B
*0B
*KB
-CB
./B
-CB
,WB
-)B
,WB
-CB
/5B
./B
.IB
.IB
/OB
0;B
/iB
0UB
1[B
2aB
3MB
3MB
2aB
2aB
3MB
2aB
2aB
2aB
4nB
5ZB
5ZB
5ZB
5tB
5tB
4nB
5tB
6zB
7fB
6zB
7�B
7fB
9rB
9XB
9�B
7�B
8�B
8�B
7�B
8�B
;B
<�B
;�B
;B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
>�B
@�B
?�B
?�B
A�B
A�B
@�B
>�B
A�B
A�B
C�B
D�B
D�B
C�B
C�B
C�B
B�B
B�B
C�B
D�B
C�B
D�B
D�B
D�B
D�B
C�B
D�B
A�B
?�B
C�B
E�B
D�B
D�B
B�B
B�B
F�B
E�B
F�B
F�B
I�B
I�B
I�B
H�B
IB
I�B
J�B
K�B
MB
LB
MB
MB
N"B
OB
Q B
QB
PB
O(B
QB
QB
RB
S&B
R B
Q4B
R B
SB
S&B
R B
R B
S&B
TB
S@B
U2B
VB
U2B
V9B
V9B
W?B
W?B
W$B
W?B
W?B
VSB
W?B
XEB
YKB
YKB
YKB
ZQB
\CB
[=B
[WB
[WB
[WB
ZQB
[=B
ZQB
ZQB
[WB
[WB
\CB
]dB
\]B
\xB
^jB
_pB
^jB
^jB
^jB
]~B
_VB
_VB
`vB
abB
a|B
`vB
_�B
`vB
cnB
dtB
cnB
dtB
dtB
dtB
dtB
cnB
b�B
a�B
a�B
c�B
d�B
dtB
f�B
f�B
ffB
f�B
f�B
ezB
e�B
e�B
dtB
ffB
f�B
d�B
f�B
g�B
g�B
f�B
f�B
gmB
g�B
g�B
g�B
h�B
h�B
h�B
h�B
jB
jB
j�B
j�B
j�B
i�B
j�B
l�B
l�B
l�B
l�B
l�B
k�B
j�B
j�B
k�B
j�B
m�B
m�B
n�B
m�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
o�B
p�B
o�B
p�B
q�B
r�B
r�B
r�B
q�B
q�B
r�B
s�B
r�B
r�B
r�B
r�B
t�B
t�B
s�B
s�B
s�B
t�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
t�B
t�B
t�B
u�B
v�B
v�B
v�B
w�B
v�B
v�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.17(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201809090033562018090900335620180909003356201809090200212018090902002120180909020021201809100023592018091000235920180910002359  JA  ARFMdecpA19c                                                                20180905093513  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180905003515  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180905003518  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180905003518  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180905003519  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180905003519  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180905003519  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180905003519  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180905003519  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180905003519                      G�O�G�O�G�O�                JA  ARUP                                                                        20180905005645                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180905153354  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20180908153356  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180908153356  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180908170021  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180909152359  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                