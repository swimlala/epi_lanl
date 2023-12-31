CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-09-25T00:35:11Z creation;2017-09-25T00:35:15Z conversion to V3.1;2019-12-19T08:00:49Z update;     
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
_FillValue                 �  I$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `t   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �L   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �,   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �D   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �LArgo profile    3.1 1.2 19500101000000  20170925003511  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_162                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�(���?�1   @�(���� @:��9Xb�d�ԕ*�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BO��BW��B`  Bh  Bp  Bx  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&  C'�fC*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DKfDK�fDL  DLy�DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS�fDT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�3D�C3D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@��\@��\AG�A=G�A]G�A}G�A���A���A���A���AΣ�Aޣ�A��A���BQ�BQ�BQ�BQ�B'Q�B/Q�B7Q�B?�RBGQ�BN�BV�B_Q�BgQ�BoQ�BwQ�BQ�B��)B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bè�BǨ�B˨�BϨ�BӨ�Bר�Bۨ�Bߨ�B��B��B��B��B��B���B���B���C�{C�{C�{C�{C	�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C!�{C#�C%�{C'��C)�{C+�{C-�{C/�{C1�{C3�{C5�{C7�{C9�{C;�{C=�{C?�{CA�{CC�{CE�{CG�{CI�{CK�{CM�{CO�{CQ�{CS�{CU�{CW�{CY�{C[�{C]�{C_�{Ca�{Cc�{Ce�{Cg�{Ci�{Ck�{Cm�{Co�{Cq�{Cs��Cu�{Cw�{Cy�{C{�{C}�{C�{C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=D uD �DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D	uD	�D
uD
�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D uD �D!uD!�D"uD"�D#uD#�D$uD$�D%uD%�D&uD&�D'uD'�D(uD(�D)uD)�D*uD*�D+uD+�D,uD,�D-uD-�D.uD.�D/uD/�D0uD0�D1uD1�D2uD2�D3uD3�D4uD4�D5uD5�D6uD6�D7uD7�D8uD8�D9uD9�D:uD:�D;uD;�D<uD<�D=uD=�D>uD>�D?uD?�D@uD@�DAuDA�DBuDB�DCuDC�DDuDD�DEuDE�DFuDF�DGuDG�DHuDH�DIuDI�DJuDJ��DK{�DK�DLn�DL�DMuDM�DNuDN�DOuDO�DPuDP�DQuDQ�DRuDR�DS{�DS�DTuDT�DUuDU�DVuDV�DWuDW�DXuDX�DYuDY�DZuDZ�D[uD[�D\uD\�D]uD]�D^uD^�D_uD_�D`uD`�DauDa�DbuDb�DcuDc�DduDd�DeuDe�DfuDf�DguDg�DhuDh�DiuDi�DjuDj�DkuDk�DluDl�DmuDm�DnuDn�DouDo�DpuDp�DquDq�DruDr�DsuDs�DtuDt�DuuDu�DvuDv�DwuDw�DxuDx�DyuDy�DzuDz�D{uD{�D|uD|�D}uD}�D~uD~�DuD�D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�=�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�DຏD���D�:�D�z�DẏD���D�:�D�z�D⺏D���D�:�D�w\D㺏D���D�:�D�z�D亏D���D�:�D�z�D序D���D�:�D�z�D溏D���D�:�D�z�D纏D���D�:�D�z�D躏D���D�:�D�z�D麏D���D�:�D�z�D꺏D���D�:�D�z�D뺏D���D�:�D�z�D캏D���D�:�D�z�D���D���D�:�D�z�DD���D�:�D�z�DﺏD���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�=�D�}�D��)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�  A���A��A��yA��mA��mA��`A��`A��`A��mA��mA��mA��mA��mA��mA��mA��mA��mA��`A��TA��/Aް!A�n�A�"�A�Aُ\Aԉ7A�{AѶFAύPAʰ!AȮA�K�Aŝ�A��A���A��A�?}A��A��A�v�A�E�A��
A�G�A��;A��A���A�ȴA��hA�ĜA�-A���A��9A���A��mA�  A��-A��\A��A�A�r�A���A�x�A�v�A�x�A�r�A��yA�oA�jA���A�=qA��\A��A�Q�A���A�dZA���A�?}A�p�A�ZA�(�A�v�A��A�-A���A�^5A�K�A�  A�ƨA�{A���A�ĜA��!A��DA��A�^5A�G�A�5?A��A���A��jA�`BA�K�A�A�ZA�I�A�;AK�A%A~�9A}��A|�+A{�A{K�A{�Ay��Ay&�Awx�Av�uAvVAuXAs�
Ar�AqAqC�Ao�^Ao�An�RAm��Al�Aj��Ai
=Ah�\Ah  Ag\)Ag�Af��AfAd(�AcO�Ac"�Ab=qA`�\A_oA^��A^z�A]�A\��A\1'A[VAY�;AW�AVz�AU+AS�AR{AQ�AP�APbAO�;AO��AOx�AN�AM�#AL�AKx�AK7LAJn�AH�yAG��AFr�AE?}AC�;ABE�AA�A@�9A@$�A?�A>r�A=��A<��A<�uA:VA9�A8�\A8I�A6�A6bA4�RA3�;A3?}A2�HA2�jA2I�A1/A0�`A0�+A/K�A.ffA-ƨA-O�A,�`A+�A*v�A)�A)S�A)oA(�A(~�A(A�A'�A&ĜA%��A$��A$�A$��A#�mA#"�A"ZA"(�A!A �HA��An�A�A=qA;dA1A\)A?}AoA9XA��A��AoA{A��A"�A�A��AAXA;dA33A33A&�A�Ar�A�AAXA
ZA	�#A	�wA	p�A�`A-A�A�A�A+A�jA5?A�-A�DA;dA E�@�|�@�M�@��#@���@�E�@��@��@��@�r�@�  @���@�h@��
@�E�@�"�@�$�@��@��@�-@�j@��@���@���@�bN@���@��#@�%@�9X@ە�@���@ٺ^@��`@�Q�@�E�@ӍP@��T@У�@�(�@�n�@���@�z�@�  @�E�@��@�
=@�p�@Ĵ9@�j@�  @�C�@�~�@��T@���@��u@�=q@���@�O�@�/@��@�%@���@�z�@�A�@�ƨ@�l�@�33@���@��@�Ĝ@���@�ȴ@�$�@��7@��@��+@�-@��^@���@�Q�@�A�@��;@��@��!@�v�@�^5@�=q@�&�@�j@��@��@��F@�;d@���@���@�V@��T@���@��@�l�@��@�J@�p�@��/@��m@��@���@��9@��w@�K�@�;d@��@��y@��\@���@�dZ@���@��H@���@�$�@��^@�x�@�?}@��j@��@�I�@���@�K�@���@��R@��\@�n�@�5?@���@��7@�/@���@�Q�@�  @��@���@���@��P@�|�@�dZ@�C�@���@�v�@�E�@�@��^@���@���@��-@�@���@�%@� �@���@�o@�$�@��7@�hs@�?}@�%@��j@��@�I�@� �@��m@�ƨ@���@�t�@�K�@�o@��@���@��R@��!@���@��\@�~�@�v�@�n�@�-@��^@�X@�%@��9@���@��u@��D@�Z@��m@�\)@�o@���@�E�@�-@���@��^@��h@�`B@�V@���@�I�@�K�@�V@���@���@��-@�@���@��#@��-@�`B@��`@��u@�I�@�1'@�(�@�  @�w@K�@
=@~��@}�@}O�@|�j@|9X@|1@{ƨ@{��@z�H@zM�@y��@yx�@x�`@xA�@wl�@v�y@v�@vv�@v{@u?}@uO�@u/@t�j@tj@s�
@sS�@sC�@r�H@r��@r~�@rJ@qX@o�@o\)@o+@nȴ@nff@m�T@m�h@m/@l�j@lz�@l1@k�F@kC�@k@j�!@j�@i��@i�7@h�u@g�@e�h@dZ@d9X@c�m@bn�@a��@a��@aX@a&�@`�`@`Ĝ@`�@`1'@`  @_�w@_K�@^ȴ@^ff@]�T@]p�@\j@\1@\1@[�m@[S�@["�@Z�H@ZM�@Y�@Y�^@Y&�@X��@X�`@XĜ@X��@X �@W|�@W�@V�R@Vv�@Vff@V5?@V{@U�@U�@T�j@Tj@TZ@TI�@T(�@Sƨ@St�@S@R-@Q�#@Q��@Q��@Qx�@Qx�@Q�7@Q�7@Qhs@P�@PA�@P1'@O�@P  @O�@Ol�@O
=@N�y@N�+@NE�@M�@M�-@M�@MV@M�@L�j@LZ@K�m@K@Jn�@J=q@JJ@JJ@JJ@I�@I�@HĜ@HA�@G�w@G��@G|�@G\)@G+@G+@G
=@F�+@E�@E�h@Ep�@E`B@E?}@EV@D�D@DI�@D(�@D1@C�m@C�F@CC�@B~�@B=q@BJ@A��@A7L@@�`@?�@?K�@?�@>��@>�R@>ff@>$�@=@=�-@=/@<��@<�@<�D@<j@<9X@<�@<1@;�m@;�
@;ƨ@;C�@:�@:^5@:J@:J@9��@9�@9��@9G�@8��@8��@8b@7��@7�w@7\)@7K�@7+@7�@6��@6�@6�+@6{@5�h@5�-@5��@5�h@5�@5O�@5/@4�@4�j@4Z@4(�@3ƨ@3��@3C�@3@2��@2~�@2^5@2M�@2M�@1��@1�^@1x�@1X@1%@0��@0�u@0r�@/�;@.�+@.E�@-�@-�-@-O�@-�@,�/@,�/@,�j@,�D@,z�@,z�@,z�@,�D@,9X@,1@+�
@+��@+��@+dZ@+o@*��@*��@*M�@*J@)��@)X@(��@'�@'��@'��@'�@'|�@'l�@'+@&��@&@%@%�h@%O�@%�@%V@%�@%�@%/@%/@%?}@%?}@%�@$�@$�@$j@$I�@$(�@#�
@#��@#o@"�H@"n�@"J@!�^@!x�@!7L@!&�@!&�@!%@ �9@ r�@ Q�@ bN@ r�@ bN@ Q�@ A�@ b@�@��@\)@
=@��@ȴ@ff@$�@�@O�@O�@/@�@I�@�m@ƨ@�@33@�!@M�@�@��@G�@�@�u@b@�w@�@l�@
=@�@v�@5?@@@O�@�@�j@��@��@�
@S�@C�@S�@dZ@S�@o@��@��@~�@�@J@�@�#@��@��@hs@G�@�@��@  @�@��@\)@;d@�@ȴ@ff@E�@{@�@�T@��@�@`B@��@�@�@��@�@j@I�@�@�m@ƨ@�F@��@t�@t�@t�@dZ@dZ@dZ@S�@dZ@S�@S�@C�@"�@
�!@
^5@
=q@
J@	��@	�@	��@	��@	X@	&�@��@��@��@��@�9@��@�@r�@A�@  @�@�@�@�@�;@�;@�w@|�@K�@+@�@��@�@��@ff@5?@{@�T@@�-@��@�h@�@p�@`B@`B@?}@?}@��@�j@I�@�@1@��@�F@dZ@33@@�@�@�H@��@��@�!@�!@~�@n�@n�@M�@�@��@�@�#@�^@��@x�@G�@7L@7L@&�@�@%@ ��@ Ĝ@ �u@ �@ A�@   1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�  A���A��A��yA��mA��mA��`A��`A��`A��mA��mA��mA��mA��mA��mA��mA��mA��mA��`A��TA��/Aް!A�n�A�"�A�Aُ\Aԉ7A�{AѶFAύPAʰ!AȮA�K�Aŝ�A��A���A��A�?}A��A��A�v�A�E�A��
A�G�A��;A��A���A�ȴA��hA�ĜA�-A���A��9A���A��mA�  A��-A��\A��A�A�r�A���A�x�A�v�A�x�A�r�A��yA�oA�jA���A�=qA��\A��A�Q�A���A�dZA���A�?}A�p�A�ZA�(�A�v�A��A�-A���A�^5A�K�A�  A�ƨA�{A���A�ĜA��!A��DA��A�^5A�G�A�5?A��A���A��jA�`BA�K�A�A�ZA�I�A�;AK�A%A~�9A}��A|�+A{�A{K�A{�Ay��Ay&�Awx�Av�uAvVAuXAs�
Ar�AqAqC�Ao�^Ao�An�RAm��Al�Aj��Ai
=Ah�\Ah  Ag\)Ag�Af��AfAd(�AcO�Ac"�Ab=qA`�\A_oA^��A^z�A]�A\��A\1'A[VAY�;AW�AVz�AU+AS�AR{AQ�AP�APbAO�;AO��AOx�AN�AM�#AL�AKx�AK7LAJn�AH�yAG��AFr�AE?}AC�;ABE�AA�A@�9A@$�A?�A>r�A=��A<��A<�uA:VA9�A8�\A8I�A6�A6bA4�RA3�;A3?}A2�HA2�jA2I�A1/A0�`A0�+A/K�A.ffA-ƨA-O�A,�`A+�A*v�A)�A)S�A)oA(�A(~�A(A�A'�A&ĜA%��A$��A$�A$��A#�mA#"�A"ZA"(�A!A �HA��An�A�A=qA;dA1A\)A?}AoA9XA��A��AoA{A��A"�A�A��AAXA;dA33A33A&�A�Ar�A�AAXA
ZA	�#A	�wA	p�A�`A-A�A�A�A+A�jA5?A�-A�DA;dA E�@�|�@�M�@��#@���@�E�@��@��@��@�r�@�  @���@�h@��
@�E�@�"�@�$�@��@��@�-@�j@��@���@���@�bN@���@��#@�%@�9X@ە�@���@ٺ^@��`@�Q�@�E�@ӍP@��T@У�@�(�@�n�@���@�z�@�  @�E�@��@�
=@�p�@Ĵ9@�j@�  @�C�@�~�@��T@���@��u@�=q@���@�O�@�/@��@�%@���@�z�@�A�@�ƨ@�l�@�33@���@��@�Ĝ@���@�ȴ@�$�@��7@��@��+@�-@��^@���@�Q�@�A�@��;@��@��!@�v�@�^5@�=q@�&�@�j@��@��@��F@�;d@���@���@�V@��T@���@��@�l�@��@�J@�p�@��/@��m@��@���@��9@��w@�K�@�;d@��@��y@��\@���@�dZ@���@��H@���@�$�@��^@�x�@�?}@��j@��@�I�@���@�K�@���@��R@��\@�n�@�5?@���@��7@�/@���@�Q�@�  @��@���@���@��P@�|�@�dZ@�C�@���@�v�@�E�@�@��^@���@���@��-@�@���@�%@� �@���@�o@�$�@��7@�hs@�?}@�%@��j@��@�I�@� �@��m@�ƨ@���@�t�@�K�@�o@��@���@��R@��!@���@��\@�~�@�v�@�n�@�-@��^@�X@�%@��9@���@��u@��D@�Z@��m@�\)@�o@���@�E�@�-@���@��^@��h@�`B@�V@���@�I�@�K�@�V@���@���@��-@�@���@��#@��-@�`B@��`@��u@�I�@�1'@�(�@�  @�w@K�@
=@~��@}�@}O�@|�j@|9X@|1@{ƨ@{��@z�H@zM�@y��@yx�@x�`@xA�@wl�@v�y@v�@vv�@v{@u?}@uO�@u/@t�j@tj@s�
@sS�@sC�@r�H@r��@r~�@rJ@qX@o�@o\)@o+@nȴ@nff@m�T@m�h@m/@l�j@lz�@l1@k�F@kC�@k@j�!@j�@i��@i�7@h�u@g�@e�h@dZ@d9X@c�m@bn�@a��@a��@aX@a&�@`�`@`Ĝ@`�@`1'@`  @_�w@_K�@^ȴ@^ff@]�T@]p�@\j@\1@\1@[�m@[S�@["�@Z�H@ZM�@Y�@Y�^@Y&�@X��@X�`@XĜ@X��@X �@W|�@W�@V�R@Vv�@Vff@V5?@V{@U�@U�@T�j@Tj@TZ@TI�@T(�@Sƨ@St�@S@R-@Q�#@Q��@Q��@Qx�@Qx�@Q�7@Q�7@Qhs@P�@PA�@P1'@O�@P  @O�@Ol�@O
=@N�y@N�+@NE�@M�@M�-@M�@MV@M�@L�j@LZ@K�m@K@Jn�@J=q@JJ@JJ@JJ@I�@I�@HĜ@HA�@G�w@G��@G|�@G\)@G+@G+@G
=@F�+@E�@E�h@Ep�@E`B@E?}@EV@D�D@DI�@D(�@D1@C�m@C�F@CC�@B~�@B=q@BJ@A��@A7L@@�`@?�@?K�@?�@>��@>�R@>ff@>$�@=@=�-@=/@<��@<�@<�D@<j@<9X@<�@<1@;�m@;�
@;ƨ@;C�@:�@:^5@:J@:J@9��@9�@9��@9G�@8��@8��@8b@7��@7�w@7\)@7K�@7+@7�@6��@6�@6�+@6{@5�h@5�-@5��@5�h@5�@5O�@5/@4�@4�j@4Z@4(�@3ƨ@3��@3C�@3@2��@2~�@2^5@2M�@2M�@1��@1�^@1x�@1X@1%@0��@0�u@0r�@/�;@.�+@.E�@-�@-�-@-O�@-�@,�/@,�/@,�j@,�D@,z�@,z�@,z�@,�D@,9X@,1@+�
@+��@+��@+dZ@+o@*��@*��@*M�@*J@)��@)X@(��@'�@'��@'��@'�@'|�@'l�@'+@&��@&@%@%�h@%O�@%�@%V@%�@%�@%/@%/@%?}@%?}@%�@$�@$�@$j@$I�@$(�@#�
@#��@#o@"�H@"n�@"J@!�^@!x�@!7L@!&�@!&�@!%@ �9@ r�@ Q�@ bN@ r�@ bN@ Q�@ A�@ b@�@��@\)@
=@��@ȴ@ff@$�@�@O�@O�@/@�@I�@�m@ƨ@�@33@�!@M�@�@��@G�@�@�u@b@�w@�@l�@
=@�@v�@5?@@@O�@�@�j@��@��@�
@S�@C�@S�@dZ@S�@o@��@��@~�@�@J@�@�#@��@��@hs@G�@�@��@  @�@��@\)@;d@�@ȴ@ff@E�@{@�@�T@��@�@`B@��@�@�@��@�@j@I�@�@�m@ƨ@�F@��@t�@t�@t�@dZ@dZ@dZ@S�@dZ@S�@S�@C�@"�@
�!@
^5@
=q@
J@	��@	�@	��@	��@	X@	&�@��@��@��@��@�9@��@�@r�@A�@  @�@�@�@�@�;@�;@�w@|�@K�@+@�@��@�@��@ff@5?@{@�T@@�-@��@�h@�@p�@`B@`B@?}@?}@��@�j@I�@�@1@��@�F@dZ@33@@�@�@�H@��@��@�!@�!@~�@n�@n�@M�@�@��@�@�#@�^@��@x�@G�@7L@7L@&�@�@%@ ��@ Ĝ@ �u@ �@ A�@   1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B~�B� B� B� B� B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�%B�VB��B��B��B�?B�B�yB�HBŢB��B�B�B�
B�B�BB�-B��B�oB�Bn�Br�Bw�Bm�B\)BK�B9XB+B�BuB  B�B�fB�5B�#B�B��B�^B��B��B��B��B��B��B��B�DB|�B�JB�Bx�Bk�BaHBYBS�BN�BD�B;dB,B�BuB1B
��B
�B
�B
�B
�yB
�/B
��B
�B
�
B
�B
��B
��B
��B
��B
��B
��B
ŢB
�dB
�RB
�FB
�!B
��B
��B
��B
�{B
�oB
�VB
�7B
�B
|�B
x�B
u�B
l�B
hsB
`BB
ZB
XB
Q�B
F�B
?}B
:^B
5?B
.B
)�B
&�B
�B
�B
%B	�B	�B	�B	�B	�B	�B	�B	�TB	�B	��B	��B	ȴB	�^B	�B	�FB	�XB	�FB	�9B	�B	��B	��B	�PB	�%B	w�B	o�B	q�B	l�B	hsB	iyB	k�B	m�B	k�B	gmB	_;B	[#B	XB	Q�B	G�B	=qB	6FB	/B	%�B	�B	�B	�B	�B	uB	bB	JB		7B	%B��B�B��B�B�B�B�mB�`B�`B�ZB�ZB�BB�#B�)B�B��B��B��B��B��BȴBBBĜBB��B�}B�wB�dB�?B�!B�!B�!B�B��B��B��B��B��B��B�\B�PB�JB�%B�B~�B~�B�B�Bz�Bt�Bs�Bp�Bp�Br�Bq�Bq�Bn�BiyB^5BgmBgmBffBe`BbNB[#BP�BXBVBQ�BQ�BS�BQ�BM�BJ�BH�BG�BD�BD�BF�BD�B@�B:^B6FB9XB;dB:^B;dB5?B33B9XB2-B+B-B5?B0!B1'B+B+B$�B)�B-B'�B#�B&�B,B'�B-B/B)�B-B/B0!B0!B0!B,B-B-B$�B!�B,B2-B5?B0!B33B9XB8RB2-B49B2-B5?B:^B>wB?}B=qB=qB>wB?}B9XB1'BC�BG�BI�BJ�BJ�BI�BI�BJ�BI�BJ�BJ�BH�BA�BL�BI�BH�BK�BL�BJ�BB�BQ�BQ�BO�BT�BYBVBS�BXBZB\)B[#BW
B[#BcTBdZBe`BcTBffBiyBhsBgmBe`BhsBl�Bo�Bp�Bs�Bv�Bu�Bx�B{�B}�B�%B�PB�hB�bB�VB�JB�%B�JB��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�-B�-B�-B�'B�RB�LB�?B�^B�dB�wBBBBB��B��B��BÖBǮBȴBɺB��B��B��B��B��BȴBȴB��B��B��B�#B�TB�ZB�ZB�`B�mB�yB�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B	B	B	B	  B	  B	B	%B	+B	
=B	VB	VB	\B	hB	hB	hB	oB	hB	bB	{B	�B	�B	"�B	#�B	$�B	&�B	'�B	)�B	,B	1'B	33B	6FB	7LB	6FB	7LB	7LB	8RB	9XB	8RB	;dB	>wB	@�B	B�B	C�B	D�B	B�B	E�B	G�B	H�B	I�B	J�B	L�B	O�B	Q�B	P�B	Q�B	Q�B	VB	XB	YB	ZB	ZB	[#B	]/B	]/B	]/B	^5B	]/B	^5B	_;B	e`B	hsB	iyB	k�B	m�B	o�B	p�B	p�B	s�B	u�B	v�B	w�B	y�B	y�B	y�B	|�B	|�B	{�B	}�B	� B	�1B	�VB	�PB	�JB	�hB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�3B	�-B	�3B	�3B	�9B	�?B	�FB	�LB	�XB	�XB	�XB	�XB	�XB	�^B	�wB	��B	��B	��B	��B	��B	��B	B	ƨB	ȴB	ɺB	ɺB	ɺB	ɺB	ɺB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�5B	�BB	�HB	�NB	�HB	�HB	�BB	�TB	�ZB	�fB	�sB	�yB	�yB	�yB	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
	7B
	7B
	7B
1B
+B
	7B
	7B
	7B
DB
JB
DB
JB
PB
VB
VB
VB
VB
PB
\B
hB
hB
oB
oB
hB
oB
hB
oB
oB
uB
oB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
#�B
%�B
%�B
%�B
&�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
(�B
(�B
'�B
,B
,B
,B
,B
-B
-B
,B
-B
/B
/B
/B
0!B
2-B
2-B
33B
33B
33B
49B
49B
33B
33B
33B
49B
49B
49B
49B
5?B
49B
49B
33B
49B
5?B
49B
5?B
5?B
5?B
5?B
49B
6FB
7LB
9XB
9XB
9XB
:^B
;dB
:^B
:^B
:^B
9XB
;dB
<jB
;dB
;dB
;dB
<jB
;dB
=qB
=qB
<jB
<jB
>wB
@�B
@�B
?}B
@�B
@�B
B�B
A�B
B�B
C�B
B�B
C�B
D�B
F�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
J�B
J�B
I�B
K�B
K�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
O�B
O�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
Q�B
Q�B
S�B
T�B
T�B
T�B
T�B
S�B
S�B
T�B
VB
VB
W
B
VB
VB
VB
VB
XB
XB
XB
W
B
XB
XB
YB
ZB
ZB
ZB
[#B
[#B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
]/B
]/B
\)B
^5B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
bNB
cTB
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
cTB
bNB
cTB
cTB
dZB
dZB
cTB
dZB
dZB
dZB
dZB
dZB
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
e`B
e`B
e`B
ffB
gmB
gmB
gmB
gmB
hsB
iyB
jB
jB
jB
jB
k�B
k�B
jB
jB
k�B
k�B
k�B
k�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
n�B
o�B
o�B
o�B
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B.B� B�B�B� B�B�B� B� B�B� B�B� B�B�'B�B�'B�B�'B�GB��B�\B��B�B�*B��B�B�wB�BοB��B�qBٚB�kB�B�BɆB�fB��B��B�mBs�Bv`B{0BqvB_pBN�B>(B/ B \B�B�B�vB�B�B��B�B�TB�.B�"B��B��B��B�B��B��B��B��B��B��Bz�Bm�Bd�B[�BU�BPHBFYB=�B.�B!HB�B
XB
�$B
��B
�MB
�!B
�B
��B
ևB
�B
׍B
�9B
�aB
�B
�[B
�NB
�BB
�jB
�+B
�<B
�>B
��B
�AB
��B
��B
�yB
�2B
��B
�(B
�rB
�[B
}�B
y�B
vzB
nIB
i_B
bB
[=B
X�B
S[B
HfB
@�B
;�B
6+B
/�B
*�B
'�B
!B
?B
KB	�nB	�cB	�oB	�[B	�!B	�cB	��B	�FB	�
B	ѝB	�gB	��B	�B	��B	��B	�DB	��B	�ZB	��B	��B	��B	�\B	�B	z*B	qB	r|B	m�B	iB	i�B	k�B	nB	l�B	iB	a-B	\B	X�B	S[B	I�B	?HB	7�B	0�B	'�B	�B	�B	�B	�B	�B	hB	�B	
XB	_B�wB�GB�tB�B�iB��B�*B�B�LB��B��B�-B�xB��B�B՛B�&B��BѷBοB�=B�MBðB�B�-B�'B� B�B�PB��B��B��B��B��B�B��B��B�NB��B��B�B�(B��B�B�uB��B�B�uB��B|jBv�BuBrBq�Bs�Br|Br-Bo�BkB`�Bg�Bg�Bf�Be�Bb�B\]BR�BYKBW$BS&BR�BTaBR�BN�BK�BI�BH�BFBE�BGzBE�BA�B<6B8B:�B<6B;JB<B6�B4TB9�B3hB,�B.B5�B1AB2GB,qB,WB&�B*�B-�B)DB%`B(>B,�B)DB-�B/�B+6B-�B/�B0�B0�B0�B-B-�B-�B&�B#�B-CB3B5�B1�B49B9�B9	B3�B5ZB3�B6FB:�B>�B@ B>(B>(B?B@ B:xB2�BC�BG�BJ	BJ�BJ�BJ#BJ	BK)BJ=BK)BK)BIlBB�BM6BJ�BI�BL~BM�BK�BC�BRTBR�BP�BU�BYeBV�BT�BXyBZkB\xB[�BW�B[�Bc�Bd�Be�Bc�Bf�Bi�Bh�Bh
BfLBi*Bm)Bp!Bq[BtTBw�Bv�By�B|�B~�B��B��B��B��B��B�B��B�jB��B�B�B�dB�-B�@B�FB�ZB�>B�XB�XB��B��B�aB�|B�|B��B��B��B��B��B��B��B��B��B��B��BªB��B��B��B��B��B�B�#B��B��B��B�B�B�RBɆB�vB҉B��BۦB�B�B�B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B�+B�FB�>B�PB�BB	'B	AB	AB	 iB	 iB	�B	tB	�B	
�B	�B	�B	�B	�B	�B	�B	�B	B	NB	B	B	�B	"�B	#�B	$�B	'B	(>B	*eB	,�B	1vB	3�B	6zB	7�B	6�B	7�B	7�B	8�B	9�B	8�B	;�B	>�B	@�B	B�B	C�B	D�B	B�B	E�B	G�B	IB	J#B	KB	M6B	PB	RB	Q4B	R:B	RTB	VB	XEB	YeB	ZkB	ZkB	[qB	]dB	]~B	]IB	^jB	]�B	^�B	_�B	e�B	h�B	i�B	k�B	m�B	o�B	p�B	p�B	s�B	vB	wB	xB	zB	z*B	zDB	}B	}<B	|jB	~wB	��B	��B	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�4B	�B	�
B	�$B	�>B	�B	�*B	�DB	�QB	�5B	�OB	�aB	�MB	�aB	�hB	��B	��B	��B	��B	��B	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�B	�"B	�B	�B	�4B	�&B	�,B	�2B	�SB	�+B	�1B	�eB	�kB	�kB	ڠB	ބB	�vB	�|B	�hB	�|B	�bB	�B	�nB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�2B	�*B	�"B	�B	�"B	�(B	�(B	�HB
;B
 4B
;B
;B
B
AB
GB
GB
B
MB
MB
3B
aB
MB
mB
EB
	RB
	lB
	lB
fB
_B
	RB
	�B
	�B
xB
~B
xB
~B
�B
�B
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
G�O�B
�B
�B
�B
B
�B
�B
 �B
 �B
 �B
#�B
%�B
%�B
%�B
'B
($B
)*B
)*B
)*B
)*B
)*B
)*B
*0B
*0B
*0B
*0B
)DB
)DB
(XB
,=B
,"B
,=B
,=B
-)B
-CB
,WB
-]B
/OB
/5B
/5B
0UB
2GB
2GB
33B
3MB
3MB
4TB
4TB
3hB
3hB
3hB
4nB
4nB
4nB
4TB
5tB
4nB
4nB
3�B
4�B
5tB
4TB
5tB
5?B
5ZB
5tB
4nB
6zB
7fB
9rB
9XB
9rB
:xB
;B
:�B
:�B
:�B
9rB
;�B
<�B
;�B
;B
;�B
<�B
;�B
=�B
=�B
<�B
<�B
>�B
@�B
@�B
?�B
@�B
@�B
B�B
A�B
B�B
C�B
B�B
C�B
D�B
F�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
J�B
J�B
J	B
K�B
LB
N�B
O�B
O�B
PB
PB
PB
QB
PB
PB
Q�B
R B
SB
S&B
S&B
S&B
S&B
S&B
R B
R:B
T,B
U2B
UB
U2B
UB
T,B
T,B
U2B
V9B
V9B
W?B
V9B
V9B
V9B
V9B
X+B
XB
X+B
W?B
XEB
X+B
YKB
Z7B
Z7B
ZQB
[=B
[=B
\)B
]IB
]IB
]IB
]/B
^OB
^OB
^OB
^5B
]IB
]dB
\]B
^jB
_pB
`vB
`BB
`\B
`\B
`vB
`vB
a|B
a|B
b�B
cnB
bhB
b�B
bNB
b�B
bhB
b�B
b�B
cTB
dtB
dtB
dtB
dtB
dtB
c�B
b�B
c�B
c�B
dZB
d�B
cnB
d�B
dtB
dtB
dtB
d�B
ezB
f�B
f�B
f�B
ffB
f�B
f�B
ffB
f�B
f�B
e�B
ezB
e�B
f�B
g�B
g�B
g�B
g�B
h�B
i�B
j�B
j�B
j�B
j�B
k�B
k�B
j�B
j�B
k�B
k�B
k�B
k�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
n�B
o�B
o�B
o�B
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.17(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201709290038492017092900384920170929003849201806221231142018062212311420180622123114201804050426312018040504263120180405042631  JA  ARFMdecpA19c                                                                20170925093507  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170925003511  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170925003512  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170925003513  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170925003514  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170925003514  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170925003514  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170925003514  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170925003514  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170925003515                      G�O�G�O�G�O�                JA  ARUP                                                                        20170925005621                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170925153216  CV  JULD            G�O�G�O�F�E�                JM  ARSQJMQC2.0                                                                 20170926000000  CF  PSAL_ADJUSTED_QCD�@ D�@ G�O�                JM  ARCAJMQC2.0                                                                 20170928153849  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170928153849  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404192631  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033114  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                