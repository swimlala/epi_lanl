CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-02-07T18:03:34Z creation      
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
resolution        =���   axis      Z        P  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  o   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  �,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �h   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �h   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230207180334  20230207180334  5906096 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7902                            2B  A   NAVIS_A                         1010                            170425                          863 @���^61   @�<���@*Cn��P�c�(�\1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"  C$  C&  C(  C*  C,  C-�fC0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ�fD[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�9�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�(�@��\@��\AG�A=G�A]G�A}G�A���A���A���A���AΣ�Aޣ�A��A���BQ�BQ�BQ�BQ�B'Q�B/Q�B7Q�B?�RBGQ�BOQ�BWQ�B_Q�BgQ�BoQ�BwQ�B�RB��)B���B���B���B���B���B���B���B���B�u�B���B���B���B���B���B���Bè�BǨ�B˨�BϨ�BӨ�Bר�Bۨ�Bߨ�B��B��B��B��B��B���B���B���C�{C�{C�{C�{C	�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�C!�{C#�{C%�{C'�{C)�{C+�{C-��C/�{C1�{C3�{C5�{C7�{C9�{C;�{C=�{C?�{CA�{CC�{CE�{CG�{CI�{CK�{CM�{CO�{CQ�{CS�{CU�{CW�{CY�{C[�{C]�{C_�{Ca�{Cc�{Ce�{Cg�{Ci�{Ck�{Cm�{Co�{Cq�{Cs�{Cu�{Cw�{Cy�{C{�{C}�{C�{C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��
C��=C��pC��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=D uD �DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D	uD	�D
uD
�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D uD �D!uD!�D"uD"�D#uD#�D$uD$�D%uD%�D&uD&�D'uD'�D(uD(�D)uD)�D*uD*�D+uD+�D,uD,�D-uD-�D.uD.�D/uD/�D0uD0�D1uD1�D2uD2�D3uD3�D4uD4�D5uD5�D6uD6�D7uD7�D8uD8�D9uD9�D:uD:�D;uD;�D<uD<�D=uD=�D>uD>�D?uD?�D@uD@�DAuDA�DBuDB�DCuDC�DDuDD�DEuDE�DFuDF�DGuDG�DHuDH�DIuDI�DJuDJ�DKuDK�DLuDL�DMuDM�DNuDN�DOuDO�DPuDP�DQuDQ�DRuDR�DSuDS�DTuDT�DUuDU�DVuDV�DWuDW�DXuDX�DYuDY�DZ{�DZ�D[uD[�D\uD\�D]uD]�D^uD^�D_uD_�D`uD`�DauDa�DbuDb�DcuDc�DduDd�DeuDe�DfuDf�DguDg�DhuDh�DiuDi�DjuDj�DkuDk�DluDl�DmuDm�DnuDn�DouDo�DpuDp�DquDq�DruDr�DsuDs�DtuDt�DuuDu�DvuDv�DwuDw�DxuDx�DyuDy�DzuDz�D{uD{�D|uD|�D}uD}�D~uD~�DuD�D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�=�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�w\D���D���D�:�D�z�D���D���D�:�D�z�D��\D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�DຏD���D�:�D�z�DẏD���D�:�D�z�D⺏D���D�:�D�z�D㺏D���D�:�D�z�D亏D���D�:�D�z�D�\D��\D�:�D�z�D溏D���D�:�D�z�D纏D���D�:�D�z�D躏D���D�:�D�z�D麏D���D�:�D�z�D꺏D���D�:�D�z�D뺏D���D�:�D�z�D캏D���D�:�D�z�D���D���D�:�D�z�DD���D�:�D�z�DﺏD���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D���D���D�4)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A�  A�7LA�$�A�-A�r�A�p�A�p�A�jA�ffA�jA�|�AΏ\AΥ�AθRAμjA�ƨAκ^AΑhA΃A�A�A�?}A�A��yA���A���A˥�A�7LA�G�A�O�A��+A���A�ĜA�  A� �A�hsA���A�1'A���A���A�ĜA��A�n�A��!A�1A���A��A��
A��A�{A�^5A�5?A�ȴA�$�A��A�ƨA�ȴA��A�=qA�G�A�9XA��;A}�
AyAwC�Av�At��An�yAd~�Ab^5A`ȴA^�A[��AX�uAT�!AR�DAO�AL��AJ �AG�#AF��AEO�AB��AA�AA&�AA+AA
=A@�HA@��A@��A@$�A@bA?�A?\)A>I�A=\)A<$�A:��A9�A8�A7��A7XA6��A5��A5&�A4�/A4A3K�A2��A1/A/�TA/�^A/��A/oA.E�A-l�A,�A,�uA,z�A,��A,ffA,9XA+x�A*�A*�A(��A(�A'��A'�7A&��A&  A$�!A$^5A#��A#`BA#VA"��A"-A"�A"1A!p�A ��A ZA��AG�A+AA�HA��Ar�AE�AA�A�A�`A�yA��A33A��AffAQ�A�;A��A?}A��AQ�A1'A�FA?}AĜAQ�A1'A��A33AoAM�AA��At�Az�AM�A�A�hA�9A�/AȴAbNA��A7LA�A �A��A��AQ�A1'A-A�A�#A�hA\)A��A��A=qA�A��A7LA
�A
��A
9XA
  A	��A	ƨA	��A	hsA	�A�A�A��A
=A��A^5A�;Al�A\)A�A~�A(�A�TA�A
=A��A��Az�A1At�AdZA\)AG�A�A �!A Q�A @�o@���@�b@�\)@��@�Ĝ@�b@�33@�M�@���@�@��@��@�(�@�|�@�
=@��@�9@�z�@�Z@�9X@��@�ȴ@@�v�@�ff@�^5@홚@�9@�Z@���@���@�ff@��@�X@��`@���@���@�j@�r�@��m@�l�@�K�@�
=@�V@�@�j@�(�@��@��m@�t�@��H@��@�7L@��D@�(�@�C�@�5?@�x�@ܴ9@�Q�@��@ۍP@��@�^5@���@ّh@���@�A�@�l�@�C�@և+@��@Չ7@�?}@��@Ԭ@�9X@Ӿw@�t�@�K�@҇+@щ7@���@мj@�Ĝ@Ѓ@ϥ�@���@�ff@��@�%@̓u@ˍP@�;d@��H@ʰ!@ʗ�@�{@��#@��#@���@�&�@�bN@Ǯ@��@�v�@�{@š�@Ĭ@ă@��@þw@Õ�@Å@�@�v�@�M�@�-@��@��h@�%@��j@�b@��@��@�dZ@�ȴ@�M�@��@���@��h@��@�x�@�p�@�p�@�hs@�G�@�V@��j@�b@�"�@���@�^5@�5?@���@���@��w@�~�@�E�@��@�J@��@�@���@�x�@�G�@��@�bN@�  @��@�ff@�-@�J@��T@���@�X@�?}@��@�Ĝ@�(�@���@�t�@�C�@�ȴ@�v�@��@��@�/@��`@�Ĝ@���@�j@�1'@�b@���@�|�@��y@�v�@�J@��^@�/@�j@��;@�\)@�o@��!@�v�@�5?@�@��T@�/@��@�r�@�(�@�b@��m@���@�;d@��@�
=@��@���@�V@�{@���@�x�@�hs@�X@��@�Ĝ@��@�1'@�  @��@���@�dZ@�;d@���@��R@�ff@��@���@��h@�O�@��`@��@�I�@� �@�|�@�@���@��\@�^5@�=q@�=q@�=q@��@�O�@��`@���@��@�(�@���@�"�@���@�ff@�E�@�=q@�J@���@�&�@��/@�I�@� �@��
@�\)@�+@�V@��^@�hs@�?}@�%@���@��@���@�bN@�1'@�b@���@���@�K�@�o@��!@�V@�@���@�O�@���@��j@��D@�z�@�r�@�1'@���@�ƨ@�C�@�@��y@�ȴ@��R@��\@�^5@�{@�@�O�@���@���@�I�@�b@�l�@�+@�"�@�
=@��R@�J@��h@�G�@�&�@���@���@�Q�@���@���@�t�@�\)@�+@���@��!@��+@�E�@�@���@��@�/@�V@��/@�z�@�bN@�1'@��@��@�b@�@|�@K�@�@~��@~@}O�@|�j@|j@|�@{ƨ@{S�@z��@z^5@zJ@y�@y��@y7L@x��@x�u@wK�@v��@vE�@v{@u�@u��@uV@t�j@tj@t(�@s�m@sC�@r�H@rM�@q��@q�^@q&�@p�`@pb@o�;@o��@o�@o
=@nȴ@n�+@m�T@m�-@mV@l��@lI�@k�m@k�F@k��@kdZ@k33@j�!@j^5@jJ@iX@i%@h��@hb@g\)@fv�@e��@e��@e/@d��@d1@cƨ@c��@b~�@bJ@a�^@ax�@aX@a&�@`��@_�@_�P@_|�@_l�@_K�@^��@^�R@^�+@^{@]p�@]�@\�j@\�D@\z�@\Z@\I�@\9X@\9X@\�@[�
@[��@[o@Z�@Z��@Z^5@ZJ@Yx�@Y&�@Y�@Y%@X��@X�@XA�@X  @W�;@W�w@Wl�@W�@W
=@V�y@V�R@V��@Vff@V5?@U��@U�h@U`B@U�@T�@Tj@T1@S��@SdZ@SC�@R�H@R=q@Qx�@Q�@P�`@PbN@Ol�@N��@N5?@M@M�-@M�-@M��@M��@M�h@M/@M�@L��@L�j@L��@L�D@Lj@LZ@Kƨ@KC�@J��@I��@I7L@H�`@HĜ@H��@H��@H��@H�u@HQ�@G�w@G
=@F$�@E��@EO�@D�D@D9X@C�m@C�F@C��@CS�@C"�@B��@B��@B�!@B�\@B�@A��@A7L@A7L@A&�@A�@A&�@A�@A�@A%@@��@@��@@�@@bN@@A�@@ �@?;d@>�@>�R@>ff@>E�@>{@=�@=��@=�@=V@<j@<(�@<(�@<�@;��@:��@:M�@:J@:J@9�@9��@9��@9hs@9G�@97L@9&�@9�@8Ĝ@8�@81'@8  @7�w@7l�@7+@6�@6ff@5�@5p�@5V@4Z@3��@3C�@2�H@2�\@2^5@2-@2-@2J@1��@1�@1�#@1��@1��@1%@01'@/��@/��@/�P@/|�@/K�@.ȴ@.5?@.$�@.{@.{@.@.@-�-@-�@,�@,��@,9X@+ƨ@+t�@*�@*��@*�!@*�\@*^5@*=q@*-@)��@)�@)�#@)hs@(��@(�u@'��@'��@'�@&V@%��@%`B@%/@$��@$z�@$�@$1@#ƨ@#�@#C�@#@"�H@"��@"�\@"M�@!��@!�#@!�^@!��@!hs@!X@!G�@!&�@!�@ ��@ bN@ 1'@ b@�;@��@�@|�@\)@;d@+@�@�@
=@�@E�@E�@5?@�T@p�@?}@V@��@�@�D@I�@(�@��@��@�@S�@�H@��@M�@J@�#@��@Ĝ@b@K�@+@+@��@�R@��@��@�+@v�@V@$�@p�@�j@Z@�@1@�m@ƨ@S�@C�@C�@C�@33@@��@�!@��@�\@n�@�@J@J@J@�@��@��@G�@%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A���A�  A�7LA�$�A�-A�r�A�p�A�p�A�jA�ffA�jA�|�AΏ\AΥ�AθRAμjA�ƨAκ^AΑhA΃A�A�A�?}A�A��yA���A���A˥�A�7LA�G�A�O�A��+A���A�ĜA�  A� �A�hsA���A�1'A���A���A�ĜA��A�n�A��!A�1A���A��A��
A��A�{A�^5A�5?A�ȴA�$�A��A�ƨA�ȴA��A�=qA�G�A�9XA��;A}�
AyAwC�Av�At��An�yAd~�Ab^5A`ȴA^�A[��AX�uAT�!AR�DAO�AL��AJ �AG�#AF��AEO�AB��AA�AA&�AA+AA
=A@�HA@��A@��A@$�A@bA?�A?\)A>I�A=\)A<$�A:��A9�A8�A7��A7XA6��A5��A5&�A4�/A4A3K�A2��A1/A/�TA/�^A/��A/oA.E�A-l�A,�A,�uA,z�A,��A,ffA,9XA+x�A*�A*�A(��A(�A'��A'�7A&��A&  A$�!A$^5A#��A#`BA#VA"��A"-A"�A"1A!p�A ��A ZA��AG�A+AA�HA��Ar�AE�AA�A�A�`A�yA��A33A��AffAQ�A�;A��A?}A��AQ�A1'A�FA?}AĜAQ�A1'A��A33AoAM�AA��At�Az�AM�A�A�hA�9A�/AȴAbNA��A7LA�A �A��A��AQ�A1'A-A�A�#A�hA\)A��A��A=qA�A��A7LA
�A
��A
9XA
  A	��A	ƨA	��A	hsA	�A�A�A��A
=A��A^5A�;Al�A\)A�A~�A(�A�TA�A
=A��A��Az�A1At�AdZA\)AG�A�A �!A Q�A @�o@���@�b@�\)@��@�Ĝ@�b@�33@�M�@���@�@��@��@�(�@�|�@�
=@��@�9@�z�@�Z@�9X@��@�ȴ@@�v�@�ff@�^5@홚@�9@�Z@���@���@�ff@��@�X@��`@���@���@�j@�r�@��m@�l�@�K�@�
=@�V@�@�j@�(�@��@��m@�t�@��H@��@�7L@��D@�(�@�C�@�5?@�x�@ܴ9@�Q�@��@ۍP@��@�^5@���@ّh@���@�A�@�l�@�C�@և+@��@Չ7@�?}@��@Ԭ@�9X@Ӿw@�t�@�K�@҇+@щ7@���@мj@�Ĝ@Ѓ@ϥ�@���@�ff@��@�%@̓u@ˍP@�;d@��H@ʰ!@ʗ�@�{@��#@��#@���@�&�@�bN@Ǯ@��@�v�@�{@š�@Ĭ@ă@��@þw@Õ�@Å@�@�v�@�M�@�-@��@��h@�%@��j@�b@��@��@�dZ@�ȴ@�M�@��@���@��h@��@�x�@�p�@�p�@�hs@�G�@�V@��j@�b@�"�@���@�^5@�5?@���@���@��w@�~�@�E�@��@�J@��@�@���@�x�@�G�@��@�bN@�  @��@�ff@�-@�J@��T@���@�X@�?}@��@�Ĝ@�(�@���@�t�@�C�@�ȴ@�v�@��@��@�/@��`@�Ĝ@���@�j@�1'@�b@���@�|�@��y@�v�@�J@��^@�/@�j@��;@�\)@�o@��!@�v�@�5?@�@��T@�/@��@�r�@�(�@�b@��m@���@�;d@��@�
=@��@���@�V@�{@���@�x�@�hs@�X@��@�Ĝ@��@�1'@�  @��@���@�dZ@�;d@���@��R@�ff@��@���@��h@�O�@��`@��@�I�@� �@�|�@�@���@��\@�^5@�=q@�=q@�=q@��@�O�@��`@���@��@�(�@���@�"�@���@�ff@�E�@�=q@�J@���@�&�@��/@�I�@� �@��
@�\)@�+@�V@��^@�hs@�?}@�%@���@��@���@�bN@�1'@�b@���@���@�K�@�o@��!@�V@�@���@�O�@���@��j@��D@�z�@�r�@�1'@���@�ƨ@�C�@�@��y@�ȴ@��R@��\@�^5@�{@�@�O�@���@���@�I�@�b@�l�@�+@�"�@�
=@��R@�J@��h@�G�@�&�@���@���@�Q�@���@���@�t�@�\)@�+@���@��!@��+@�E�@�@���@��@�/@�V@��/@�z�@�bN@�1'@��@��@�b@�@|�@K�@�@~��@~@}O�@|�j@|j@|�@{ƨ@{S�@z��@z^5@zJ@y�@y��@y7L@x��@x�u@wK�@v��@vE�@v{@u�@u��@uV@t�j@tj@t(�@s�m@sC�@r�H@rM�@q��@q�^@q&�@p�`@pb@o�;@o��@o�@o
=@nȴ@n�+@m�T@m�-@mV@l��@lI�@k�m@k�F@k��@kdZ@k33@j�!@j^5@jJ@iX@i%@h��@hb@g\)@fv�@e��@e��@e/@d��@d1@cƨ@c��@b~�@bJ@a�^@ax�@aX@a&�@`��@_�@_�P@_|�@_l�@_K�@^��@^�R@^�+@^{@]p�@]�@\�j@\�D@\z�@\Z@\I�@\9X@\9X@\�@[�
@[��@[o@Z�@Z��@Z^5@ZJ@Yx�@Y&�@Y�@Y%@X��@X�@XA�@X  @W�;@W�w@Wl�@W�@W
=@V�y@V�R@V��@Vff@V5?@U��@U�h@U`B@U�@T�@Tj@T1@S��@SdZ@SC�@R�H@R=q@Qx�@Q�@P�`@PbN@Ol�@N��@N5?@M@M�-@M�-@M��@M��@M�h@M/@M�@L��@L�j@L��@L�D@Lj@LZ@Kƨ@KC�@J��@I��@I7L@H�`@HĜ@H��@H��@H��@H�u@HQ�@G�w@G
=@F$�@E��@EO�@D�D@D9X@C�m@C�F@C��@CS�@C"�@B��@B��@B�!@B�\@B�@A��@A7L@A7L@A&�@A�@A&�@A�@A�@A%@@��@@��@@�@@bN@@A�@@ �@?;d@>�@>�R@>ff@>E�@>{@=�@=��@=�@=V@<j@<(�@<(�@<�@;��@:��@:M�@:J@:J@9�@9��@9��@9hs@9G�@97L@9&�@9�@8Ĝ@8�@81'@8  @7�w@7l�@7+@6�@6ff@5�@5p�@5V@4Z@3��@3C�@2�H@2�\@2^5@2-@2-@2J@1��@1�@1�#@1��@1��@1%@01'@/��@/��@/�P@/|�@/K�@.ȴ@.5?@.$�@.{@.{@.@.@-�-@-�@,�@,��@,9X@+ƨ@+t�@*�@*��@*�!@*�\@*^5@*=q@*-@)��@)�@)�#@)hs@(��@(�u@'��@'��@'�@&V@%��@%`B@%/@$��@$z�@$�@$1@#ƨ@#�@#C�@#@"�H@"��@"�\@"M�@!��@!�#@!�^@!��@!hs@!X@!G�@!&�@!�@ ��@ bN@ 1'@ b@�;@��@�@|�@\)@;d@+@�@�@
=@�@E�@E�@5?@�T@p�@?}@V@��@�@�D@I�@(�@��@��@�@S�@�H@��@M�@J@�#@��@Ĝ@b@K�@+@+@��@�R@��@��@�+@v�@V@$�@p�@�j@Z@�@1@�m@ƨ@S�@C�@C�@C�@33@@��@�!@��@�\@n�@�@J@J@J@�@��@��@G�@%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�+B	�1B	�7B	�7B	�7B	�=B	��B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�LB	�LB	�^B	�^B	�LB	�RB
=qB
�B
�7B
�PB
�hB
��B
��B
ÖB�B�B(�B5?BR�BZB_;B^5BgmBq�Bu�B|�B�1B��B��B��B��B�PB�Bm�B]/BN�BN�B_;BS�B<jB1'B�B
�B
�^B
�B
��B
�VB
x�B
[#B
?}B
2-B
.B
�B
%B	�5B	��B	��B	ƨB	�dB	�wB	B	��B	��B
Q�B
^5B
gmB
cTB
iyB
}�B
�bB
��B
�B
�qB
��B
�B
�;B
�B
�B
��BJB�B�B\BDBB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��BB+B
=B1B+B
=BVB�B�B�B�BoBVB%B  B
��B
��B
��B
�B
�yB
�fB
�ZB
�NB
�HB
�NB
�BB
�BB
�TB
�NB
�;B
�5B
�B
�B
�B
�
B
�B
�B
�B
�B
�
B
�B
�B
�B
�
B
�B
�HB
�HB
�5B
�NB
�BB
�;B
�5B
�)B
�#B
�#B
�#B
�#B
�#B
�#B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ɺB
��B
��B
��B
��B
��B
��B
ȴB
ǮB
ƨB
ĜB
ÖB
ÖB
ÖB
B
��B
��B
��B
�wB
�wB
�qB
�jB
�dB
�^B
�^B
�XB
�RB
�RB
�RB
�LB
�FB
�9B
�3B
�!B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�{B
�uB
�hB
�\B
�VB
�PB
�PB
�JB
�JB
�DB
�DB
�DB
�7B
�7B
�1B
�1B
�%B
�%B
�%B
�B
�B
�B
�B
�B
�B
�B
�B
�B
� B
� B
~�B
~�B
~�B
}�B
}�B
|�B
}�B
}�B
}�B
~�B
}�B
}�B
� B
�B
}�B
|�B
~�B
�B
�B
�B
~�B
~�B
{�B
{�B
{�B
z�B
x�B
w�B
v�B
u�B
t�B
u�B
v�B
v�B
v�B
w�B
v�B
v�B
u�B
t�B
u�B
u�B
s�B
t�B
t�B
t�B
t�B
s�B
t�B
s�B
s�B
r�B
r�B
s�B
r�B
r�B
r�B
q�B
r�B
r�B
q�B
r�B
r�B
q�B
q�B
q�B
p�B
q�B
q�B
q�B
p�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
t�B
t�B
r�B
r�B
r�B
s�B
t�B
w�B
x�B
y�B
z�B
|�B
|�B
|�B
{�B
z�B
y�B
z�B
{�B
}�B
}�B
|�B
|�B
|�B
|�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
}�B
~�B
~�B
�B
� B
� B
� B
� B
� B
�B
�B
�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�1B
�+B
�+B
�+B
�%B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�7B
�=B
�=B
�=B
�DB
�=B
�DB
�=B
�=B
�DB
�JB
�DB
�JB
�DB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�VB
�PB
�VB
�PB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�VB
�\B
�bB
�bB
�hB
�bB
�hB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
�uB
�{B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�'B
�'B
�'B
�'B
�'B
�!B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�-B
�-B
�-B
�-B
�-B
�-B
�3B
�3B
�9B
�9B
�9B
�9B
�?B
�?B
�?B
�?B
�FB
�FB
�FB
�FB
�FB
�FB
�FB
�LB
�LB
�LB
�LB
�LB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�^B
�^B
�^B
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�jB
�jB
�jB
�jB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�wB
�wB
�wB
�wB
�wB
�wB
�}B
�}B
�}B
�}B
��B
��B
��B
��B
��B
��B
B
B
ÖB
B
B
ÖB
ÖB
B
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ĜB
ĜB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ƨB
ƨB
ǮB
ǮB
ǮB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ɺB
ɺB
ɺB
ɺB
ɺB
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�
B
�
B
�
B
�
B
�B
�
B
�
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�#B
�#B
�#B
�)B
�)B
�)B
�)B
�/B
�/B
�/B
�/B
�5B
�5B
�5B
�;B
�;B
�;B
�;B
�;B
�BB
�BB
�BB
�BB
�HB
�HB
�HB
�HB
�HB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�TB
�TB
�TB
�TB
�TB
�TB
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�`B
�`B
�`B
�`B
�`B
�`B
�fB
�`B
�fB
�fB
�fB
�fB
�fB
�mB
�fB
�mB
�sB
�yB
�yB
�sB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�B
�B
�B
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
�B
�33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333B	�+B	�1B	�7B	�7B	�7B	�=B	��B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�LB	�LB	�^B	�^B	�LB	�RB
=qB
�B
�7B
�PB
�hB
��B
��B
ÖB�B�B(�B5?BR�BZB_;B^5BgmBq�Bu�B|�B�1B��B��B��B��B�PB�Bm�B]/BN�BN�B_;BS�B<jB1'B�B
�B
�^B
�B
��B
�VB
x�B
[#B
?}B
2-B
.B
�B
%B	�5B	��B	��B	ƨB	�dB	�wB	B	��B	��B
Q�B
^5B
gmB
cTB
iyB
}�B
�bB
��B
�B
�qB
��B
�B
�;B
�B
�B
��BJB�B�B\BDBB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��BB+B
=B1B+B
=BVB�B�B�B�BoBVB%B  B
��B
��B
��B
�B
�yB
�fB
�ZB
�NB
�HB
�NB
�BB
�BB
�TB
�NB
�;B
�5B
�B
�B
�B
�
B
�B
�B
�B
�B
�
B
�B
�B
�B
�
B
�B
�HB
�HB
�5B
�NB
�BB
�;B
�5B
�)B
�#B
�#B
�#B
�#B
�#B
�#B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ɺB
��B
��B
��B
��B
��B
��B
ȴB
ǮB
ƨB
ĜB
ÖB
ÖB
ÖB
B
��B
��B
��B
�wB
�wB
�qB
�jB
�dB
�^B
�^B
�XB
�RB
�RB
�RB
�LB
�FB
�9B
�3B
�!B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�{B
�uB
�hB
�\B
�VB
�PB
�PB
�JB
�JB
�DB
�DB
�DB
�7B
�7B
�1B
�1B
�%B
�%B
�%B
�B
�B
�B
�B
�B
�B
�B
�B
�B
� B
� B
~�B
~�B
~�B
}�B
}�B
|�B
}�B
}�B
}�B
~�B
}�B
}�B
� B
�B
}�B
|�B
~�B
�B
�B
�B
~�B
~�B
{�B
{�B
{�B
z�B
x�B
w�B
v�B
u�B
t�B
u�B
v�B
v�B
v�B
w�B
v�B
v�B
u�B
t�B
u�B
u�B
s�B
t�B
t�B
t�B
t�B
s�B
t�B
s�B
s�B
r�B
r�B
s�B
r�B
r�B
r�B
q�B
r�B
r�B
q�B
r�B
r�B
q�B
q�B
q�B
p�B
q�B
q�B
q�B
p�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
t�B
t�B
r�B
r�B
r�B
s�B
t�B
w�B
x�B
y�B
z�B
|�B
|�B
|�B
{�B
z�B
y�B
z�B
{�B
}�B
}�B
|�B
|�B
|�B
|�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
}�B
~�B
~�B
�B
� B
� B
� B
� B
� B
�B
�B
�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�1B
�+B
�+B
�+B
�%B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�7B
�=B
�=B
�=B
�DB
�=B
�DB
�=B
�=B
�DB
�JB
�DB
�JB
�DB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�VB
�PB
�VB
�PB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�VB
�\B
�bB
�bB
�hB
�bB
�hB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
�uB
�{B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�'B
�'B
�'B
�'B
�'B
�!B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�-B
�-B
�-B
�-B
�-B
�-B
�3B
�3B
�9B
�9B
�9B
�9B
�?B
�?B
�?B
�?B
�FB
�FB
�FB
�FB
�FB
�FB
�FB
�LB
�LB
�LB
�LB
�LB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�^B
�^B
�^B
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�jB
�jB
�jB
�jB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�wB
�wB
�wB
�wB
�wB
�wB
�}B
�}B
�}B
�}B
��B
��B
��B
��B
��B
��B
B
B
ÖB
B
B
ÖB
ÖB
B
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ĜB
ĜB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ƨB
ƨB
ǮB
ǮB
ǮB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ɺB
ɺB
ɺB
ɺB
ɺB
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�
B
�
B
�
B
�
B
�B
�
B
�
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�#B
�#B
�#B
�)B
�)B
�)B
�)B
�/B
�/B
�/B
�/B
�5B
�5B
�5B
�;B
�;B
�;B
�;B
�;B
�BB
�BB
�BB
�BB
�HB
�HB
�HB
�HB
�HB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�TB
�TB
�TB
�TB
�TB
�TB
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�`B
�`B
�`B
�`B
�`B
�`B
�fB
�`B
�fB
�fB
�fB
�fB
�fB
�mB
�fB
�mB
�sB
�yB
�yB
�sB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�B
�B
�B
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
�B
�33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.17 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230207180334                              AO  ARCAADJP                                                                    20230207180334    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230207180334  QCP$                G�O�G�O�G�O�1B83E           AO  ARGQQCPL                                                                    20230207180334  QCF$                G�O�G�O�G�O�8000            