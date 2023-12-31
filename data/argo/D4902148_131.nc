CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-03-19T15:35:22Z creation;2018-03-19T15:35:25Z conversion to V3.1;2019-12-18T07:24:11Z update;2022-11-21T05:31:04Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
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
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �P   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ΐ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180319153522  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_131                     2C  DdENAVIS_A                         0397                            ARGO 011514                     863 @�T�gY  1   @�T�5� @<2���m�dE�	�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�0 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@u�@�\)@��\AG�A=G�A]G�A}G�A���A���A���A���AΣ�Aޣ�A��A���BQ�BQ�BQ�BQ�B'Q�B/Q�B7Q�B?Q�BGQ�BOQ�BWQ�B_Q�BgQ�BoQ�BwQ�BQ�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bè�BǨ�B˨�BϨ�BӨ�Bר�Bۨ�Bߨ�B��B��B��B��B��B���B���B���C�{C�{C�{C�{C	�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C!�{C#�{C%�{C'�{C)�{C+�{C-�{C/�{C1�{C3�{C5�{C7�{C9�{C;�{C=�{C?�{CA�{CC�{CE�{CG�{CI�{CK�{CM�{CO�{CQ�{CS�{CU�{CW�{CY�{C[�{C]�{C_�{Ca�{Cc�{Ce�{Cg�{Ci�{Ck�{Cm�{Co�{Cq�{Cs�{Cu�{Cw�{Cy�{C{�{C}�{C�{C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=D uD �DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D	uD	�D
uD
�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D uD �D!uD!�D"uD"�D#uD#�D$uD$�D%uD%�D&uD&�D'uD'�D(uD(�D)uD)�D*uD*�D+uD+�D,uD,�D-uD-�D.uD.�D/uD/�D0uD0�D1uD1�D2uD2�D3uD3�D4uD4�D5uD5�D6uD6�D7uD7�D8uD8�D9uD9�D:uD:�D;uD;�D<uD<�D=uD=�D>uD>�D?uD?�D@uD@�DAuDA�DBuDB�DCuDC�DDuDD�DEuDE�DFuDF�DGuDG�DHuDH�DIuDI�DJuDJ�DKuDK�DLuDL�DMuDM�DNuDN�DOuDO�DPuDP�DQuDQ�DRuDR�DSuDS�DTuDT�DUuDU�DVuDV�DWuDW�DXuDX�DYuDY�DZuDZ�D[uD[�D\uD\�D]uD]�D^uD^�D_uD_�D`uD`�DauDa�DbuDb�DcuDc�DduDd�DeuDe�DfuDf�DguDg�DhuDh�DiuDi�DjuDj�DkuDk�DluDl�DmuDm�DnuDn�DouDo�DpuDp�DquDq�DruDr�DsuDs�DtuDt�DuuDu�DvuDv�DwuDw�DxuDx�DyuDy�DzuDz�D{uD{�D|uD|�D}uD}�D~uD~�DuD�D�:�D�w\D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�7\D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�}�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�DຏD���D�:�D�z�DẏD���D�:�D�z�D⺏D���D�:�D�z�D㺏D���D�:�D�z�D亏D���D�:�D�z�D序D���D�:�D�z�D溏D���D�:�D�z�D纏D���D�:�D�z�D躏D���D�:�D�z�D麏D���D�:�D�z�D꺏D���D�:�D�z�D뺏D���D�:�D�z�D캏D���D�:�D�z�D���D���D�:�D�z�DD���D�:�D�z�DﺏD���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�*�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A�x�A�z�A�~�A�x�A�v�A�|�A��A��A��A��DA��\A��hA��\A��hA��hA���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ZA���A�+A�\)A���A��7A�|�A�XA��A�jA�(�A��A��hA�S�A�1A�7LA��RA�r�A���A��A�x�A�jA�A��A�A���A�?}A�5?A���A�Q�A���A��yA�^5A�
=A�A�x�A��A���A���A�ȴA��A�-A��`A���A�n�A���A�1'A�%A���A��A�XA~��A}l�Az��Ay�Av�\At�As�mAs��As��As��AshsAs;dAr��ApffAo�AoK�An�Aml�Akx�Ah�Ag�Acl�Aa�A`��A`1A_p�A_33A^M�A\r�A[��A[/AY�;AX��AXbAW?}AV��AV��AU��AU�AS��ASC�AR��AQVAPQ�AOƨAO��AOhsAO"�AN��AM33ALQ�AKt�AK%AJ��AJM�AJ{AH�AHAF��AD�AD-ACS�AA�PA@{A?
=A>�A=;dA<�yA<ĜA<M�A;�^A;%A:M�A:�A9��A9&�A8��A8��A8��A8r�A7;dA6z�A6ffA6jA6Q�A6{A5��A4��A41'A3�hA2��A2�A2VA1VA0M�A/��A/dZA.��A,^5A+�FA+�A+
=A)C�A( �A'��A't�A'VA&n�A%��A%t�A#A"��A"�A"�+A"ffA"5?A!�
A!|�A ��A �!A JA�Ar�A�RAƨAC�AZA��A?}A��A��A �A�PA��A��A9XA�A`BAz�A  A+A�uA{A�A$�A�7Av�Ap�AdZA33A
�9A	\)AI�A�^A�A�AVA�
A7LA =q@��#@���@��;@��@�J@���@��@�o@�ff@���@��h@�/@��/@�@�@��@�o@���@���@陚@��`@�F@�^5@�X@�r�@�~�@���@���@�bN@�S�@�E�@�?}@�A�@ە�@��H@��@ٲ-@�z�@�dZ@�v�@Ԭ@�ff@Ѻ^@�33@�V@�5?@�5?@�{@ͩ�@�O�@���@�1'@˥�@�;d@ʗ�@ɩ�@���@�C�@�J@�(�@þw@�;d@\@�@�&�@��u@�r�@�z�@�Z@���@�l�@���@���@�V@�bN@�  @��;@��F@�K�@���@���@�r�@��m@�~�@��@���@��j@�"�@��@�ȴ@��#@��@��@�-@�G�@�b@�ƨ@��@���@���@��R@�=q@��@�@�&�@���@��u@�r�@�A�@���@��F@�l�@��@��R@���@���@��H@�v�@�J@��T@���@�(�@��
@��P@�33@���@�~�@�J@���@��
@��@��@�33@��@�v�@��7@�bN@���@�"�@���@�E�@�J@�@��-@���@��h@�x�@�O�@��@��9@�9X@��m@��w@���@�t�@�;d@�o@���@�@�&�@�%@��`@���@��j@��9@��@���@�r�@�  @�+@�$�@�{@�J@�@���@�X@��@���@���@�A�@� �@�b@�1@�  @��@��@��;@��;@��w@�t�@�dZ@�;d@�33@�"�@��@�ȴ@��R@���@���@��\@��+@�ff@��#@���@�O�@���@���@�bN@~�+@~{@}�@}�T@}@}�-@}��@}�@}�@}O�@}O�@}O�@}/@|��@|��@{ƨ@{"�@zn�@zJ@y��@y��@y�7@y%@x��@xbN@x1'@xb@w;d@v��@u�T@u�h@up�@u�@t�D@t1@sƨ@s��@st�@s33@s@r��@r�!@r^5@r-@q��@q�#@q�^@qx�@p��@p�@pA�@p  @o�@o|�@o\)@o
=@nv�@m�-@m�h@m�h@m�h@m�@mO�@m?}@l��@k�
@k�F@k33@j�H@j��@jn�@h��@hb@g|�@g;d@g�@g
=@fȴ@f�+@f{@e@e�@e`B@eO�@d�@d�j@d�j@d��@dz�@dj@c�
@b�@ax�@`�u@_|�@_
=@^��@^��@^�@^ȴ@^�R@^��@^�+@^v�@^V@^E�@^$�@^{@]�@]��@]@]��@]O�@\�/@[�
@[t�@[S�@[C�@[@Z��@Z�!@Zn�@Y��@Y7L@XĜ@W;d@VV@V$�@U@U�h@Up�@U?}@T�/@St�@S"�@S@R�H@RJ@Q��@Q�^@Q��@Qx�@QX@Q7L@P��@PĜ@P�u@PbN@P1'@P  @O�@O�;@O��@O�@OK�@NV@M��@MV@L��@Lj@Lj@L�@Kƨ@K�@J�H@K@Ko@J��@J=q@JM�@J-@I�^@Ix�@I&�@H��@HbN@H  @G��@G�w@G��@G��@G\)@GK�@G+@G
=@F��@E�-@D�@D�/@D�/@D��@D��@D�j@D�j@D�j@D�@Dj@C�m@C�@C33@B��@B��@B=q@A��@A�7@AX@A7L@A&�@@��@@A�@>�R@>�+@>5?@=�h@=?}@=/@<�@<9X@;�m@;"�@;@:��@:�@9��@9��@9��@9��@9G�@97L@9%@8�`@8Ĝ@8�u@8b@7��@7K�@7
=@6�@6�R@6��@6v�@6ff@65?@6{@5�T@5�-@5��@5�h@5�h@5�@5p�@5/@4��@4��@4(�@3�F@3�@2�H@1��@0��@0�@/��@/l�@/+@/
=@.�y@.ȴ@.ȴ@.�R@.��@.��@.�+@.v�@.V@.E�@.$�@-�T@-�-@-p�@-�@,�@,�j@,��@,�D@,z�@,z�@,z�@,z�@,j@,Z@,I�@,I�@,I�@,(�@+�m@+�F@+��@+��@+�@+33@*�H@*��@*��@*�!@*^5@*�@)��@)%@(Q�@(b@'�P@'K�@'+@'�@'
=@&��@&�@&��@&��@&�+@&ff@&V@&{@%�@%�@%��@%@%@%�-@%p�@%�@$��@$�D@$j@$I�@$I�@$I�@$�@"�!@"=q@!�@!��@!X@ �`@ �9@ �@ A�@ 1'@ 1'@ 1'@ 1'@ 1'@ 1'@  �@�@��@�@l�@\)@K�@
=@�R@��@�+@�+@ff@5?@$�@{@@@�T@@��@�@?}@��@Z@(�@�m@�F@��@�@t�@�@C�@o@@�H@�!@��@��@��@��@~�@~�@M�@M�@M�@=q@=q@J@�#@�7@G�@Q�@�;@�y@$�@{@{@@�@��@@�-@�-@�@`B@`B@/@�@V@�@��@z�@�F@o@~�@��@7L@%@��@Ĝ@�u@bN@�@|�@\)@;d@�@�@�y@��@�+@��@�+@V@5?@5?@5?@$�@$�@{@�@�@�@�@�T@@�h@O�@�@��@�/@�@Z@�
@�@dZ@C�@
��@
�\@
~�@
~�@
^5@
-@	�@	�7@	G�@	�@	%@�9@�@bN@bN@Q�@A�@b@�@\)@;d@�R@ff@V@@�T@�T@@�@p�@p�@p�@`B@/@��@��@�j@�D@z�@I�@(�@��@ƨ@�
@ƨ@��@�@t�@C�@o@@��@~�@��@��@��@�7@�7@x�@G�@ ��@ ��@ ��@ �`@ ��@ �9@ ��@ ��@ �u@ �u@ �u@ �u@ �@ r�@ A�@  �?��w?�;d?�;d1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A�x�A�z�A�~�A�x�A�v�A�|�A��A��A��A��DA��\A��hA��\A��hA��hA���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ZA���A�+A�\)A���A��7A�|�A�XA��A�jA�(�A��A��hA�S�A�1A�7LA��RA�r�A���A��A�x�A�jA�A��A�A���A�?}A�5?A���A�Q�A���A��yA�^5A�
=A�A�x�A��A���A���A�ȴA��A�-A��`A���A�n�A���A�1'A�%A���A��A�XA~��A}l�Az��Ay�Av�\At�As�mAs��As��As��AshsAs;dAr��ApffAo�AoK�An�Aml�Akx�Ah�Ag�Acl�Aa�A`��A`1A_p�A_33A^M�A\r�A[��A[/AY�;AX��AXbAW?}AV��AV��AU��AU�AS��ASC�AR��AQVAPQ�AOƨAO��AOhsAO"�AN��AM33ALQ�AKt�AK%AJ��AJM�AJ{AH�AHAF��AD�AD-ACS�AA�PA@{A?
=A>�A=;dA<�yA<ĜA<M�A;�^A;%A:M�A:�A9��A9&�A8��A8��A8��A8r�A7;dA6z�A6ffA6jA6Q�A6{A5��A4��A41'A3�hA2��A2�A2VA1VA0M�A/��A/dZA.��A,^5A+�FA+�A+
=A)C�A( �A'��A't�A'VA&n�A%��A%t�A#A"��A"�A"�+A"ffA"5?A!�
A!|�A ��A �!A JA�Ar�A�RAƨAC�AZA��A?}A��A��A �A�PA��A��A9XA�A`BAz�A  A+A�uA{A�A$�A�7Av�Ap�AdZA33A
�9A	\)AI�A�^A�A�AVA�
A7LA =q@��#@���@��;@��@�J@���@��@�o@�ff@���@��h@�/@��/@�@�@��@�o@���@���@陚@��`@�F@�^5@�X@�r�@�~�@���@���@�bN@�S�@�E�@�?}@�A�@ە�@��H@��@ٲ-@�z�@�dZ@�v�@Ԭ@�ff@Ѻ^@�33@�V@�5?@�5?@�{@ͩ�@�O�@���@�1'@˥�@�;d@ʗ�@ɩ�@���@�C�@�J@�(�@þw@�;d@\@�@�&�@��u@�r�@�z�@�Z@���@�l�@���@���@�V@�bN@�  @��;@��F@�K�@���@���@�r�@��m@�~�@��@���@��j@�"�@��@�ȴ@��#@��@��@�-@�G�@�b@�ƨ@��@���@���@��R@�=q@��@�@�&�@���@��u@�r�@�A�@���@��F@�l�@��@��R@���@���@��H@�v�@�J@��T@���@�(�@��
@��P@�33@���@�~�@�J@���@��
@��@��@�33@��@�v�@��7@�bN@���@�"�@���@�E�@�J@�@��-@���@��h@�x�@�O�@��@��9@�9X@��m@��w@���@�t�@�;d@�o@���@�@�&�@�%@��`@���@��j@��9@��@���@�r�@�  @�+@�$�@�{@�J@�@���@�X@��@���@���@�A�@� �@�b@�1@�  @��@��@��;@��;@��w@�t�@�dZ@�;d@�33@�"�@��@�ȴ@��R@���@���@��\@��+@�ff@��#@���@�O�@���@���@�bN@~�+@~{@}�@}�T@}@}�-@}��@}�@}�@}O�@}O�@}O�@}/@|��@|��@{ƨ@{"�@zn�@zJ@y��@y��@y�7@y%@x��@xbN@x1'@xb@w;d@v��@u�T@u�h@up�@u�@t�D@t1@sƨ@s��@st�@s33@s@r��@r�!@r^5@r-@q��@q�#@q�^@qx�@p��@p�@pA�@p  @o�@o|�@o\)@o
=@nv�@m�-@m�h@m�h@m�h@m�@mO�@m?}@l��@k�
@k�F@k33@j�H@j��@jn�@h��@hb@g|�@g;d@g�@g
=@fȴ@f�+@f{@e@e�@e`B@eO�@d�@d�j@d�j@d��@dz�@dj@c�
@b�@ax�@`�u@_|�@_
=@^��@^��@^�@^ȴ@^�R@^��@^�+@^v�@^V@^E�@^$�@^{@]�@]��@]@]��@]O�@\�/@[�
@[t�@[S�@[C�@[@Z��@Z�!@Zn�@Y��@Y7L@XĜ@W;d@VV@V$�@U@U�h@Up�@U?}@T�/@St�@S"�@S@R�H@RJ@Q��@Q�^@Q��@Qx�@QX@Q7L@P��@PĜ@P�u@PbN@P1'@P  @O�@O�;@O��@O�@OK�@NV@M��@MV@L��@Lj@Lj@L�@Kƨ@K�@J�H@K@Ko@J��@J=q@JM�@J-@I�^@Ix�@I&�@H��@HbN@H  @G��@G�w@G��@G��@G\)@GK�@G+@G
=@F��@E�-@D�@D�/@D�/@D��@D��@D�j@D�j@D�j@D�@Dj@C�m@C�@C33@B��@B��@B=q@A��@A�7@AX@A7L@A&�@@��@@A�@>�R@>�+@>5?@=�h@=?}@=/@<�@<9X@;�m@;"�@;@:��@:�@9��@9��@9��@9��@9G�@97L@9%@8�`@8Ĝ@8�u@8b@7��@7K�@7
=@6�@6�R@6��@6v�@6ff@65?@6{@5�T@5�-@5��@5�h@5�h@5�@5p�@5/@4��@4��@4(�@3�F@3�@2�H@1��@0��@0�@/��@/l�@/+@/
=@.�y@.ȴ@.ȴ@.�R@.��@.��@.�+@.v�@.V@.E�@.$�@-�T@-�-@-p�@-�@,�@,�j@,��@,�D@,z�@,z�@,z�@,z�@,j@,Z@,I�@,I�@,I�@,(�@+�m@+�F@+��@+��@+�@+33@*�H@*��@*��@*�!@*^5@*�@)��@)%@(Q�@(b@'�P@'K�@'+@'�@'
=@&��@&�@&��@&��@&�+@&ff@&V@&{@%�@%�@%��@%@%@%�-@%p�@%�@$��@$�D@$j@$I�@$I�@$I�@$�@"�!@"=q@!�@!��@!X@ �`@ �9@ �@ A�@ 1'@ 1'@ 1'@ 1'@ 1'@ 1'@  �@�@��@�@l�@\)@K�@
=@�R@��@�+@�+@ff@5?@$�@{@@@�T@@��@�@?}@��@Z@(�@�m@�F@��@�@t�@�@C�@o@@�H@�!@��@��@��@��@~�@~�@M�@M�@M�@=q@=q@J@�#@�7@G�@Q�@�;@�y@$�@{@{@@�@��@@�-@�-@�@`B@`B@/@�@V@�@��@z�@�F@o@~�@��@7L@%@��@Ĝ@�u@bN@�@|�@\)@;d@�@�@�y@��@�+@��@�+@V@5?@5?@5?@$�@$�@{@�@�@�@�@�T@@�h@O�@�@��@�/@�@Z@�
@�@dZ@C�@
��@
�\@
~�@
~�@
^5@
-@	�@	�7@	G�@	�@	%@�9@�@bN@bN@Q�@A�@b@�@\)@;d@�R@ff@V@@�T@�T@@�@p�@p�@p�@`B@/@��@��@�j@�D@z�@I�@(�@��@ƨ@�
@ƨ@��@�@t�@C�@o@@��@~�@��@��@��@�7@�7@x�@G�@ ��@ ��@ ��@ �`@ ��@ �9@ ��@ ��@ �u@ �u@ �u@ �u@ �@ r�@ A�@  �?��w?�;d?�;d1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��BɺBŢBB�}B��B�qB�LB��B��B��B��B�{B�bB�Bx�Bp�BaHBK�BC�B7LB0!B �BVB+B��B�fB�B��B�dB��B�hB�7B�1B~�BiyBYBS�BH�BC�B?}B2-B�B
=B  B
�B
�BB
��B
�-B
��B
�VB
�B
r�B
e`B
T�B
E�B
D�B
D�B
D�B
C�B
A�B
>wB
9XB
,B
&�B
#�B
�B
�B
JB	��B	�B	�B	��B	ȴB	ƨB	ÖB	��B	�jB	�3B	�B	�B	��B	��B	��B	��B	�{B	�oB	�VB	�JB	�1B	�B	�B	w�B	q�B	n�B	l�B	k�B	iyB	e`B	]/B	YB	T�B	Q�B	O�B	M�B	K�B	E�B	@�B	9XB	1'B	,B	%�B	�B	�B	oB	\B	
=B		7B	1B	B	B��B��B��B��B��B��B��B��B�B�B�B�B�B�yB�sB�fB�NB�BB�/B�#B�B�B��B��B��B��BǮB��B�qB�jB�XB�9B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�PB�1B�%B�B�B� B}�B|�B{�By�Bw�Bt�Bq�Bo�Bm�Bl�BiyBhsBffBdZBcTB`BB^5B\)BZBXBW
BVBS�BQ�BO�BM�BJ�BH�BD�BB�B@�B?}B=qB<jB<jB;dB:^B9XB8RB8RB7LB7LB7LB6FB6FB5?B49B33B1'B.B-B,B)�B'�B%�B%�B%�B(�B(�B(�B(�B)�B,B-B-B,B,B,B+B(�B'�B&�B&�B(�B(�B0!B5?B6FB6FB6FB6FB6FB6FB7LB8RB9XB:^B;dB<jB<jB;dB;dB;dB<jB=qB=qB?}B?}B@�BA�BB�BB�BC�BD�BE�BF�BG�BH�BH�BH�BH�BI�BK�BK�BJ�BK�BK�BK�BL�BK�BK�BJ�BK�BL�BR�B^5BgmBm�Bl�Bn�Br�Bw�Bz�B{�B|�B|�B~�B� B�B�B�B�B�B�B�%B�+B�1B�hB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�-B�RB�wBÖBŢBȴBɺB��B��B��B��B��B��B��B��B��B��B�
B�B�B�B�#B�)B�)B�sB�B�B�B�B��B��B��B��B��B��B��B	B	%B	%B	+B		7B	\B	{B	�B	�B	�B	�B	�B	 �B	 �B	!�B	!�B	"�B	"�B	#�B	%�B	%�B	'�B	'�B	(�B	+B	,B	,B	-B	-B	.B	.B	.B	33B	49B	7LB	:^B	<jB	>wB	H�B	J�B	J�B	J�B	K�B	K�B	L�B	L�B	L�B	M�B	M�B	M�B	M�B	N�B	P�B	S�B	VB	YB	[#B	[#B	\)B	]/B	_;B	`BB	bNB	bNB	cTB	ffB	hsB	k�B	m�B	m�B	n�B	q�B	s�B	t�B	u�B	u�B	w�B	x�B	x�B	y�B	z�B	{�B	|�B	}�B	}�B	~�B	�B	�B	�B	�B	�B	�%B	�%B	�1B	�=B	�DB	�JB	�JB	�JB	�JB	�JB	�PB	�\B	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�-B	�XB	�dB	�}B	ĜB	ŢB	ŢB	ŢB	ƨB	ƨB	ƨB	ǮB	ǮB	ǮB	ȴB	ȴB	ȴB	ȴB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�#B	�)B	�BB	�TB	�TB	�ZB	�`B	�`B	�`B	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
  B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
1B

=B
JB
JB
JB
JB
JB
JB
JB
JB
JB
PB
VB
\B
bB
hB
hB
oB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
)�B
)�B
+B
-B
.B
/B
0!B
1'B
1'B
2-B
2-B
2-B
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
49B
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
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
;dB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
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
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
D�B
E�B
E�B
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
H�B
H�B
I�B
I�B
I�B
I�B
I�B
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
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
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
Q�B
Q�B
R�B
R�B
S�B
T�B
VB
XB
XB
XB
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
[#B
\)B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
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
bNB
cTB
bNB
cTB
cTB
cTB
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
e`B
ffB
ffB
ffB
ffB
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
jB
jB
jB
jB
jB
jB
jB
k�B
l�B
l�B
m�B
m�B
m�B
n�B
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
q�B
q�B
r�B
r�B
r�B
r�B
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
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B��B��B��B�B�B�B�B�B��B��B�B�B�B�B��B�B�B��B�B�B�B�B�B�B�B��B�2B�gB��B�@B�NB̈́BȀB�-B��B�B�]B��B��B��B��B�YB��B� B�SBy�BrGBc BMjBE�B9	B1�B#B�B�B�B��B�KB��B��B�5B�TB�	B�#B�'Bk�BZQBU�BI�BEBB�B5?B \B�B�B
��B
�B
ԕB
��B
�!B
�}B
�B
u%B
hXB
WYB
FB
D�B
D�B
EB
DB
BB
?�B
;�B
-B
'�B
$�B
 �B
KB
vB	�wB	�B	�kB	�B	ɆB	�zB	�MB	��B	�wB	�9B	�;B	��B	�LB	��B	��B	�$B	�MB	��B	�BB	��B	�RB	�9B	��B	x�B	rGB	n�B	l�B	l=B	j�B	gB	^�B	Z7B	U�B	R�B	P}B	N�B	MjB	G+B	BuB	;dB	2|B	-�B	($B	�B	�B	uB	�B	
�B		�B	�B	B	B��B�^B�rB��B�2B�+B�+B�tB�B�B��B�B��B�B�DB�B�B�B�B��B�	BٴB�B��B��B�PB�#B��B�B��B�B��B��B��B��B��B�B��B��B��B�B�B��B�)B�=B�QB�eB�+B��B��B� B�vB��B�B�3B��B��B~�B}�B|�Bz�ByrBv�Br�BpUBn}Bm�BjBi�BgRBeFBd�Ba�B_pB]�B[=BX_BW�BW
BU�BS[BQ BOBBL�BJ�BGzBC�BB'BA B>]B=<B="B<6B:�B:DB9XB8�B7�B7�B7�B6�B6�B6�B5�B4�B2|B0B-�B,�B+B)B&�B&�B'8B)�B)�B)�B)�B*�B,�B-�B-�B,�B,�B,�B+�B)�B(�B(sB(sB)�B*B0�B5tB6zB6�B6�B6�B6�B6�B7�B8�B:B;0B<�B=<B=�B<�B;�B<B=B>B>B@ B?�B@�BA�BCBC-BDgBESBFYBGEBHBIBIBIRBI�BJ�BLJBL~BK�BMBM6BM�BM�BL0BL0BK�BL�BNBS�B_BhXBnBm]Bo�BtBx�B{dB|PB}VB}�BcB�OB�UB�;B�uB�aB��B��B��B��B��B� B��B��B��B�eB�IB�!B�B�-B�NB�:B�ZB��B��B�]B�]B�}B��B��B�B�>B�B�B�%B�B�	B�B��B��B�B�B�"B�BB�bB�oB�MB�YB�EB�_B�eB�qBܒB�/B��B��B��B��B��B��B��B��B�B�`B��B��B	SB	YB	YB	zB		�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	!�B	!�B	#B	# B	$B	&B	&2B	($B	($B	)DB	+6B	,=B	,=B	-CB	-CB	.IB	.cB	.�B	3hB	4�B	7�B	:�B	<�B	?HB	H�B	J�B	J�B	J�B	K�B	K�B	MB	MB	L�B	M�B	NB	NB	N"B	O(B	Q4B	TaB	VSB	Y1B	[WB	[qB	\]B	]dB	_pB	`vB	b�B	b�B	c�B	f�B	h�B	k�B	m�B	m�B	o B	q�B	s�B	t�B	u�B	u�B	xB	y	B	y	B	y�B	z�B	|B	}B	~(B	~BB	HB	�;B	�AB	�GB	�MB	�SB	�YB	�tB	�fB	��B	�xB	�JB	�dB	�dB	�~B	�~B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	�8B	�$B	�0B	�6B	�=B	�=B	�IB	�5B	�OB	�UB	�UB	��B	��B	��B	��B	� B	��B	żB	żB	��B	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�0B	�B	� B	� B	�&B	�,B	�,B	�2B	�MB	�YB	یB	��B	�B	�B	�B	�B	�zB	�zB	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�6B	��B	��B	�(B	�BB
 B	�.B
 OB
 B
UB
[B
aB
MB
SB
SB
SB
?B
?B
YB
?B
_B
_B
�B

�B
dB
JB
dB
JB
dB
dB
dB
dB
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
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
"B
# B
$B
$B
$�B
$�B
%B
%B
&B
&B
&B
&B
'B
'B
'B
&�B
'B
'B
'B
($B
(>B
)DB
)DB
*0B
*KB
+kB
-wB
.IB
/iB
0UB
1[B
1AB
2aB
2GB
2GB
2GB
2GB
2GB
2GB
3MB
3hB
3hB
3hB
3hB
3MB
4nB
4nB
5tB
5tB
5tB
5ZB
5?B
6`B
6`B
6`B
6`B
6`B
6FB
6`B
6`B
6zB
6`B
7�B
7fB
7�B
7�B
7�B
8lB
8�B
8lB
8�B
8�B
9�B
9�B
9�B
;�B
<�B
<�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
?}B
?�B
?�B
?�B
?�B
@�B
@�B
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
B�B
D�B
E�B
E�B
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
H�B
H�B
I�B
I�B
I�B
I�B
I�B
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
MB
L�B
MB
MB
MB
M�B
OB
OB
PB
PB
O�B
O�B
O�B
PB
PB
Q B
QB
QB
Q B
QB
Q B
Q B
QB
RB
R B
RB
Q�B
RB
RB
R B
RB
R B
S@B
S@B
TFB
UgB
VSB
X+B
X+B
XB
X+B
X+B
X+B
XB
X+B
XEB
XEB
Y1B
YKB
Y1B
YKB
YKB
YKB
YeB
YB
ZkB
[qB
\xB
]dB
^OB
^OB
_VB
_pB
_pB
_�B
`\B
a|B
abB
a|B
abB
a|B
a|B
bhB
bhB
bhB
b�B
bhB
bNB
cnB
b�B
cnB
cTB
cnB
cTB
cnB
cnB
cnB
c�B
c�B
c�B
dtB
dtB
d�B
d�B
d�B
e�B
f�B
f�B
f�B
f�B
g�B
h�B
hsB
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
l�B
l�B
m�B
m�B
m�B
n�B
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
q�B
q�B
r�B
r�B
r�B
r�B
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
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.17(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201803300034222018033000342220180330003422202211182134042022111821340420221118213404201804031939472018040319394720180403193947  JA  ARFMdecpA19c                                                                20180320003521  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180319153522  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180319153523  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180319153524  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180319153524  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180319153524  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180319153524  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180319153524  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180319153525  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180319153525                      G�O�G�O�G�O�                JA  ARUP                                                                        20180319155658                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180319153231  CV  JULD            G�O�G�O�F¥                JM  ARCAJMQC2.0                                                                 20180329153422  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180329153422  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103947  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171535                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123404  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                