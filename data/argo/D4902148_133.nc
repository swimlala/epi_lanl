CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-04-08T15:36:21Z creation;2018-04-08T15:36:24Z conversion to V3.1;2019-12-18T07:23:44Z update;2022-11-21T05:30:58Z update;     
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
_FillValue                 �  ]T   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180408153621  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_133                     2C  DdF5NAVIS_A                         0397                            ARGO 011514                     863 @�Y�L�A 1   @�Y��@y�@<e��O��dF5?|�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@���A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111���@{�@�\)@��\AG�A;�A]G�A}G�A���A���A���A���AΣ�Aޣ�A��A���BQ�BQ�BQ�BQ�B'Q�B/Q�B7Q�B?Q�BGQ�BOQ�BWQ�B_Q�BgQ�BoQ�BwQ�BQ�B���B���B���B���B���B���B��)B���B���B���B���B���B���B���B���B���Bè�BǨ�B˨�BϨ�BӨ�Bר�Bۨ�Bߨ�B��B��B��B��B��B���B���B���C�{C�C�{C�{C	�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C!�{C#�{C%�{C'�{C)�{C+�{C-�{C/�{C1�{C3�{C5�{C7�{C9�{C;�{C=�{C?�{CA�{CC�{CE�{CG�{CI�{CK�{CM�CO�{CQ�{CS�{CU�{CW�{CY�{C[�{C]�{C_�{Ca�{Cc�{Ce�{Cg�{Ci�{Ck�{Cm�{Co�{Cq�{Cs�{Cu�{Cw�{Cy�{C{�{C}�{C�{C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=D uD �DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D	uD	�D
uD
�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D uD �D!uD!�D"uD"�D#uD#�D$uD$�D%uD%�D&uD&�D'uD'�D(uD(�D)uD)�D*uD*�D+uD+�D,uD,�D-uD-�D.uD.�D/uD/�D0uD0�D1uD1�D2uD2�D3uD3�D4uD4�D5uD5�D6uD6�D7uD7�D8uD8�D9uD9�D:uD:�D;uD;�D<uD<�D=uD=�D>uD>�D?uD?�D@uD@�DAuDA�DBuDB�DCuDC�DDuDD�DEuDE�DFuDF�DGuDG�DHuDH�DIuDI�DJuDJ�DKuDK�DLuDL�DMuDM�DNuDN�DOuDO�DPuDP�DQuDQ�DRuDR�DSuDS�DTuDT�DUuDU�DVuDV�DWuDW�DXuDX�DYuDY�DZuDZ�D[uD[�D\uD\�D]uD]�D^uD^�D_uD_�D`uD`�DauDa�DbuDb�DcuDc�DduDd�DeuDe�DfuDf�DguDg�DhuDh�DiuDi�DjuDj�DkuDk�DluDl�DmuDm�DnuDn�DouDo�DpuDp�DquDq�DruDr�DsuDs�DtuDt�DuuDu�DvuDv�DwuDw�DxuDx�DyuDy�DzuDz�D{uD{�D|uD|�D}uD}�D~uD~�DuD�D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�=�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�=�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�DຏD���D�:�D�z�DẏD���D�:�D�z�D⺏D���D�:�D�z�D㺏D���D�:�D�z�D亏D���D�:�D�z�D序D���D�:�D�z�D溏D���D�:�D�z�D纏D���D�:�D�z�D躏D���D�:�D�z�D麏D���D�:�D�z�D꺏D���D�:�D�z�D뺏D���D�:�D�z�D캏D���D�:�D�z�D���D���D�:�D�z�DD���D�:�D�z�DﺏD���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�=�D�}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��/A�1A�
=A�{A��A��A�$�A�$�A� �A� �A��A��A��A��A��A��A��A��A��A��A��A�{A�bA�oA�oA���A���A���A���A��yA���A��-A�5?A��/A�I�A���A�%A��9A��\A��A�dZA��A���A��A�1'A���A��A���A��uA�JA���A�C�A���A��A��-A��jA�|�A�G�A��hA�C�A��A��A��A��uA�hsA��+A���A�Q�A���A��!A�ZA�{A���A��A�&�A���A��HA�Q�A�z�A�z�A�mA~�A~�A}/A{Az-AyS�Ax-Aw�wAwC�AwoAv�/Av �At�yAtz�ArffAq\)Ap~�Ao�Anz�Am33Am
=Al�jAl=qAj �AhffAg��Ag
=Ae�TAdA�Ac�-Ac�Ab�\AbAaVA`^5A_�A_%A]�A[t�AYoAX1'AW��AWoAV�HAV��AVv�AV$�AT�AR�RAQ��AQ��AQS�AP�AL��AJ��AJVAIx�AHȴAGl�AF�AFn�AE�ADv�AC�AC�FAC\)AC�AB��AB5?AAt�A@�yA@M�A@A?t�A>��A>A�A>bA=�A;��A;;dA;+A:��A:��A:�A9/A8bA7�PA77LA6��A6A�A4v�A4{A3�A2�!A2^5A0��A0�A/�mA/O�A/VA.��A.n�A-�A,z�A+�A*�\A(�A&A�A%�A%hsA$ȴA$M�A#��A"�`A"�+A"{A!�PA �A ȴA r�A �A�PA�HA��A�A-A��A�AffA�#A�A-A�A
=AffA�A�A%A�uA5?AhsA��A��A
�HA��A�Al�AM�A�A��A\)A�A��A��A^5AM�A(�A�Ap�AĜA�;A|�A33@���@�G�@���@�ȴ@�=q@���@�7L@���@�&�@�bN@�(�@�P@�@�@�@�@�%@�bN@�|�@��@�w@���@�@�j@ް!@�{@�@�/@��/@܃@�9X@�t�@ڰ!@�?}@Ցh@Ӆ@��@Ұ!@��#@�Z@�|�@�V@̬@�+@�E�@��#@ɡ�@�7L@�Ĝ@�I�@�9X@Ǖ�@�M�@�(�@�X@�?}@�&�@��`@�Q�@� �@�+@�@�z�@�K�@�  @��`@�b@�dZ@���@���@���@�j@� �@�  @��F@�S�@�+@�@��@���@���@���@��+@�M�@��7@��;@���@�%@�bN@�1@���@�~�@���@�Q�@�b@��@�ƨ@��F@���@�t�@�\)@�;d@��@��y@�ff@�$�@���@���@��9@�r�@�1@���@�"�@���@�-@��h@��@��@�9X@��w@�S�@�v�@���@��@�p�@�hs@��@��u@�I�@�(�@��@�1@��m@���@�;d@���@�E�@�5?@���@��@�p�@�X@�7L@��@���@�33@��+@�=q@�@��#@��^@��7@��`@�|�@�dZ@�
=@���@�=q@��T@��-@���@���@�p�@�X@��@��`@��u@�b@���@��H@���@�^5@���@�`B@�&�@�r�@���@�ƨ@���@�;d@��H@���@�ff@�5?@��@��^@���@��h@��@�?}@��`@���@�Ĝ@��j@��9@���@�bN@�1'@��
@���@���@���@���@��P@�C�@���@��\@�n�@�5?@�$�@��@��T@��h@�hs@�O�@�%@���@��9@��@�9X@�w@
=@~��@~v�@~V@~@}��@|1@z�\@zn�@zM�@y�@yhs@yG�@y7L@y&�@y%@w�w@w�@v�+@u�h@t9X@s�m@s��@st�@sC�@s@r�H@r�H@r�!@r=q@q�@q��@qX@q&�@q%@p�`@pbN@p1'@o�;@o|�@o;d@n�y@nȴ@n�R@n�+@nV@m@m�@l��@l��@lj@l�@kS�@kC�@k"�@j��@jn�@jJ@i�^@i�^@i��@i��@ix�@i7L@i�@h�`@hĜ@h��@hbN@h1'@g�@g��@g;d@f��@fE�@ep�@eV@d�/@d��@d�@d��@d��@d��@d�D@d�D@dz�@d(�@c��@c"�@b��@bM�@a��@ahs@`r�@_�@_�P@^ȴ@^��@^ff@^@]�T@]��@]p�@]�@\�j@\Z@\�@[�m@[�F@[��@[t�@Z��@Z~�@Z~�@ZM�@Z-@Y��@Y�#@Y��@YG�@X��@X�@Xr�@XA�@X �@Xb@X  @W�w@W|�@W\)@W;d@W�@V�R@V5?@U�@U�@UV@T��@T�@Tj@S��@So@R��@RM�@R-@RJ@Q�@Q�#@Q�^@Qhs@Q%@P�u@P�@P�@PbN@PbN@PQ�@PQ�@PQ�@PQ�@PA�@PA�@P1'@P �@Pb@O�@N�y@M�h@M�@M`B@MV@L�@L�j@Lz�@L�@KC�@J�\@J=q@I��@Ix�@I7L@HĜ@HQ�@H �@G�P@FV@E��@D�@Dz�@D9X@C��@CdZ@C"�@Co@B�@B��@B�!@B�\@Bn�@B^5@B�@A�#@A��@A��@A��@@��@@bN@@b@?��@?��@?l�@?\)@?;d@?�@>�y@>�@>�R@>�+@>�+@>E�@=�@=�-@=�@=O�@<�/@<I�@<1@;�m@;�
@;��@;33@;o@:�@:��@:M�@:�@:J@9�#@9�^@9x�@9&�@9%@8�u@8b@7�P@7;d@7�@7
=@6��@6�y@6�@6�R@5�@5�h@5�@4�j@4�D@3��@3�@333@3"�@3@2�@2�H@2�!@2�\@2�@1�#@1��@1��@1hs@0��@0r�@01'@0b@0b@/�@/��@/��@/;d@.�@.ȴ@.��@.E�@.5?@.$�@.$�@.$�@-�@-�-@-�-@-�@-`B@-V@,��@,�@,j@+�F@*�\@*J@)��@)�@'�;@'l�@'+@'
=@&ff@&@%�-@%O�@%?}@%/@%�@$�/@$I�@#�m@#��@#�@"��@"n�@"n�@"^5@"^5@"^5@"M�@"M�@"J@!�^@!��@!hs@!&�@ Ĝ@ bN@�w@�@�@�R@��@�+@�+@�+@v�@ff@5?@$�@$�@{@@{@{@@@�T@�@?}@��@�@z�@I�@(�@�m@ƨ@ƨ@�F@�F@��@dZ@33@��@n�@^5@^5@=q@��@�^@�7@X@G�@7L@7L@&�@�`@�9@�u@�@r�@Q�@�@\)@�@
=@��@�y@��@$�@��@�h@`B@�@�@z�@��@ƨ@��@��@dZ@33@"�@o@o@�@�@�@�H@�H@��@��@�\@M�@J@�^@�7@hs@hs@X@X@G�@7L@��@��@�9@�@ �@��@��@�@��@�P@|�@K�@K�@+@��@ȴ@��@��@E�@@?}@��@�/@�/@��@I�@�m@�
@�
@ƨ@��@dZ@
��@
�\@
M�@	�@	�7@	x�@	hs@	X@	X@	G�@	7L@	7L@	&�@	&�@	&�@	�@��@�@r�@Q�@  @�@l�@l�@K�@+@�@
=@��@�y@�@ȴ@�R@��@��@��@ff@E�@$�@@��@�h@p�@`B@O�@?}@/@V@��@�D@I�@I�@9X@�@��@�F@��@S�@C�@"�@o@@��@�!@~�@n�@�@�@��@��@��@�^@X@%@ ��@ Ĝ@ Ĝ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��/A�1A�
=A�{A��A��A�$�A�$�A� �A� �A��A��A��A��A��A��A��A��A��A��A��A�{A�bA�oA�oA���A���A���A���A��yA���A��-A�5?A��/A�I�A���A�%A��9A��\A��A�dZA��A���A��A�1'A���A��A���A��uA�JA���A�C�A���A��A��-A��jA�|�A�G�A��hA�C�A��A��A��A��uA�hsA��+A���A�Q�A���A��!A�ZA�{A���A��A�&�A���A��HA�Q�A�z�A�z�A�mA~�A~�A}/A{Az-AyS�Ax-Aw�wAwC�AwoAv�/Av �At�yAtz�ArffAq\)Ap~�Ao�Anz�Am33Am
=Al�jAl=qAj �AhffAg��Ag
=Ae�TAdA�Ac�-Ac�Ab�\AbAaVA`^5A_�A_%A]�A[t�AYoAX1'AW��AWoAV�HAV��AVv�AV$�AT�AR�RAQ��AQ��AQS�AP�AL��AJ��AJVAIx�AHȴAGl�AF�AFn�AE�ADv�AC�AC�FAC\)AC�AB��AB5?AAt�A@�yA@M�A@A?t�A>��A>A�A>bA=�A;��A;;dA;+A:��A:��A:�A9/A8bA7�PA77LA6��A6A�A4v�A4{A3�A2�!A2^5A0��A0�A/�mA/O�A/VA.��A.n�A-�A,z�A+�A*�\A(�A&A�A%�A%hsA$ȴA$M�A#��A"�`A"�+A"{A!�PA �A ȴA r�A �A�PA�HA��A�A-A��A�AffA�#A�A-A�A
=AffA�A�A%A�uA5?AhsA��A��A
�HA��A�Al�AM�A�A��A\)A�A��A��A^5AM�A(�A�Ap�AĜA�;A|�A33@���@�G�@���@�ȴ@�=q@���@�7L@���@�&�@�bN@�(�@�P@�@�@�@�@�%@�bN@�|�@��@�w@���@�@�j@ް!@�{@�@�/@��/@܃@�9X@�t�@ڰ!@�?}@Ցh@Ӆ@��@Ұ!@��#@�Z@�|�@�V@̬@�+@�E�@��#@ɡ�@�7L@�Ĝ@�I�@�9X@Ǖ�@�M�@�(�@�X@�?}@�&�@��`@�Q�@� �@�+@�@�z�@�K�@�  @��`@�b@�dZ@���@���@���@�j@� �@�  @��F@�S�@�+@�@��@���@���@���@��+@�M�@��7@��;@���@�%@�bN@�1@���@�~�@���@�Q�@�b@��@�ƨ@��F@���@�t�@�\)@�;d@��@��y@�ff@�$�@���@���@��9@�r�@�1@���@�"�@���@�-@��h@��@��@�9X@��w@�S�@�v�@���@��@�p�@�hs@��@��u@�I�@�(�@��@�1@��m@���@�;d@���@�E�@�5?@���@��@�p�@�X@�7L@��@���@�33@��+@�=q@�@��#@��^@��7@��`@�|�@�dZ@�
=@���@�=q@��T@��-@���@���@�p�@�X@��@��`@��u@�b@���@��H@���@�^5@���@�`B@�&�@�r�@���@�ƨ@���@�;d@��H@���@�ff@�5?@��@��^@���@��h@��@�?}@��`@���@�Ĝ@��j@��9@���@�bN@�1'@��
@���@���@���@���@��P@�C�@���@��\@�n�@�5?@�$�@��@��T@��h@�hs@�O�@�%@���@��9@��@�9X@�w@
=@~��@~v�@~V@~@}��@|1@z�\@zn�@zM�@y�@yhs@yG�@y7L@y&�@y%@w�w@w�@v�+@u�h@t9X@s�m@s��@st�@sC�@s@r�H@r�H@r�!@r=q@q�@q��@qX@q&�@q%@p�`@pbN@p1'@o�;@o|�@o;d@n�y@nȴ@n�R@n�+@nV@m@m�@l��@l��@lj@l�@kS�@kC�@k"�@j��@jn�@jJ@i�^@i�^@i��@i��@ix�@i7L@i�@h�`@hĜ@h��@hbN@h1'@g�@g��@g;d@f��@fE�@ep�@eV@d�/@d��@d�@d��@d��@d��@d�D@d�D@dz�@d(�@c��@c"�@b��@bM�@a��@ahs@`r�@_�@_�P@^ȴ@^��@^ff@^@]�T@]��@]p�@]�@\�j@\Z@\�@[�m@[�F@[��@[t�@Z��@Z~�@Z~�@ZM�@Z-@Y��@Y�#@Y��@YG�@X��@X�@Xr�@XA�@X �@Xb@X  @W�w@W|�@W\)@W;d@W�@V�R@V5?@U�@U�@UV@T��@T�@Tj@S��@So@R��@RM�@R-@RJ@Q�@Q�#@Q�^@Qhs@Q%@P�u@P�@P�@PbN@PbN@PQ�@PQ�@PQ�@PQ�@PA�@PA�@P1'@P �@Pb@O�@N�y@M�h@M�@M`B@MV@L�@L�j@Lz�@L�@KC�@J�\@J=q@I��@Ix�@I7L@HĜ@HQ�@H �@G�P@FV@E��@D�@Dz�@D9X@C��@CdZ@C"�@Co@B�@B��@B�!@B�\@Bn�@B^5@B�@A�#@A��@A��@A��@@��@@bN@@b@?��@?��@?l�@?\)@?;d@?�@>�y@>�@>�R@>�+@>�+@>E�@=�@=�-@=�@=O�@<�/@<I�@<1@;�m@;�
@;��@;33@;o@:�@:��@:M�@:�@:J@9�#@9�^@9x�@9&�@9%@8�u@8b@7�P@7;d@7�@7
=@6��@6�y@6�@6�R@5�@5�h@5�@4�j@4�D@3��@3�@333@3"�@3@2�@2�H@2�!@2�\@2�@1�#@1��@1��@1hs@0��@0r�@01'@0b@0b@/�@/��@/��@/;d@.�@.ȴ@.��@.E�@.5?@.$�@.$�@.$�@-�@-�-@-�-@-�@-`B@-V@,��@,�@,j@+�F@*�\@*J@)��@)�@'�;@'l�@'+@'
=@&ff@&@%�-@%O�@%?}@%/@%�@$�/@$I�@#�m@#��@#�@"��@"n�@"n�@"^5@"^5@"^5@"M�@"M�@"J@!�^@!��@!hs@!&�@ Ĝ@ bN@�w@�@�@�R@��@�+@�+@�+@v�@ff@5?@$�@$�@{@@{@{@@@�T@�@?}@��@�@z�@I�@(�@�m@ƨ@ƨ@�F@�F@��@dZ@33@��@n�@^5@^5@=q@��@�^@�7@X@G�@7L@7L@&�@�`@�9@�u@�@r�@Q�@�@\)@�@
=@��@�y@��@$�@��@�h@`B@�@�@z�@��@ƨ@��@��@dZ@33@"�@o@o@�@�@�@�H@�H@��@��@�\@M�@J@�^@�7@hs@hs@X@X@G�@7L@��@��@�9@�@ �@��@��@�@��@�P@|�@K�@K�@+@��@ȴ@��@��@E�@@?}@��@�/@�/@��@I�@�m@�
@�
@ƨ@��@dZ@
��@
�\@
M�@	�@	�7@	x�@	hs@	X@	X@	G�@	7L@	7L@	&�@	&�@	&�@	�@��@�@r�@Q�@  @�@l�@l�@K�@+@�@
=@��@�y@�@ȴ@�R@��@��@��@ff@E�@$�@@��@�h@p�@`B@O�@?}@/@V@��@�D@I�@I�@9X@�@��@�F@��@S�@C�@"�@o@@��@�!@~�@n�@�@�@��@��@��@�^@X@%@ ��@ Ĝ@ Ĝ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�7B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�{B�{B�oB�oB�oB�hB�bB�\B�PB�DB�7B�+B�+B�7B�1B�7B�7B�+B�B�B� Bz�Bq�BbNBT�BN�BG�B?}B+B�BB�`B��B��B�}B�3B��B�BgmB`BB]/BYBJ�B.B1B
��B
�B
�B
�fB
�;B
�/B
�
B
��B
ÖB
�^B
�B
��B
��B
�oB
�DB
�B
v�B
p�B
jB
dZB
`BB
^5B
\)B
ZB
VB
M�B
J�B
>wB
7LB
1'B
+B
$�B
�B
�B
�B
{B
1B	��B	��B	��B	�B	�ZB	�BB	�/B	�B	�B	��B	��B	ȴB	ÖB	�dB	�!B	��B	��B	��B	��B	��B	��B	��B	�{B	�DB	�B	|�B	z�B	w�B	o�B	^5B	T�B	P�B	L�B	H�B	B�B	?}B	<jB	9XB	33B	0!B	/B	-B	,B	)�B	&�B	#�B	 �B	�B	�B	�B	�B	{B	oB	\B	1B	B	B	B	B��B��B�B�B�B�B�B�`B�NB�BB�)B�B��B��B��B��B��BɺBŢB��B�dB�RB�3B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�hB�VB�JB�7B�+B�B�B�B~�B{�Bx�Bt�Bo�Bm�BjBgmBdZBbNB`BB]/BYBS�BP�BP�BN�BM�BL�BK�BK�BK�BK�BJ�BJ�BJ�BI�BI�BH�BG�BE�BC�BB�BA�B@�B>wB=qB=qB=qB<jB:^B6FB2-B1'B1'B1'B0!B/B/B.B.B-B+B+B+B+B+B)�B(�B(�B(�B(�B(�B(�B)�B,B,B-B0!B33B33B33B33B49B49B5?B7LB9XB:^B;dB;dB;dB<jB<jB<jB;dB<jB>wBB�BB�BB�BB�BC�BC�BD�BE�BF�BF�BI�BO�BQ�BQ�BVBW
BW
BXBYBYBZBZB[#B[#B\)B\)B\)B\)B\)B\)B^5BbNBgmBjBk�Bl�Bl�Bo�Bu�Bw�Bx�By�By�By�Bz�Bz�B{�B{�B{�B|�B~�B~�B� B�B�B�B�%B�%B�7B�DB�JB�VB�bB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�-B�-B�3B�9B�RB�dB�wB�}B��B��B��BBƨB��B��B��B��B�B�#B�/B�/B�5B�;B�;B�HB�NB�ZB�mB�B�B�B�B��B��B��B��B	B	B	B	+B		7B	JB	VB	bB	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	$�B	$�B	$�B	$�B	&�B	)�B	-B	.B	0!B	0!B	1'B	2-B	5?B	7LB	7LB	9XB	;dB	<jB	=qB	?}B	B�B	E�B	G�B	G�B	H�B	I�B	J�B	P�B	XB	XB	YB	ZB	\)B	]/B	]/B	]/B	]/B	bNB	e`B	gmB	k�B	p�B	r�B	s�B	t�B	u�B	v�B	v�B	v�B	w�B	x�B	z�B	{�B	|�B	}�B	~�B	~�B	�B	�B	�B	�%B	�+B	�1B	�7B	�7B	�=B	�DB	�PB	�bB	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�!B	�'B	�'B	�'B	�'B	�'B	�'B	�-B	�9B	�FB	�LB	�XB	�^B	�jB	�}B	��B	B	ŢB	ŢB	ƨB	ǮB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�/B	�5B	�5B	�;B	�;B	�;B	�BB	�NB	�TB	�ZB	�`B	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
%B
+B
	7B

=B
JB
PB
VB
bB
hB
hB
hB
oB
oB
oB
oB
uB
uB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
#�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
)�B
+B
+B
,B
,B
-B
.B
.B
.B
.B
.B
/B
.B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
33B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
6FB
7LB
9XB
:^B
:^B
;dB
=qB
>wB
>wB
>wB
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
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
H�B
I�B
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
K�B
K�B
K�B
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
O�B
P�B
P�B
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
S�B
S�B
S�B
S�B
S�B
S�B
T�B
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
^5B
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
aHB
`BB
`BB
aHB
aHB
aHB
aHB
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
e`B
e`B
e`B
e`B
e`B
ffB
ffB
gmB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
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
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
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
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�RB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�pB�JB��B��B�KB��B��B��B��B�1B��B��B��B|Bs�BdZBVBPHBIRBB[B-]B vB�B�B�mBңB��B�`B�B��BhsB`�B^jB[�BOvB2�B
rB
��B
�B
�QB
�RB
��B
�5B
�yB
��B
�SB
��B
�UB
�4B
��B
��B
��B
�YB
w�B
q�B
k�B
eB
`�B
^�B
\�B
[=B
WsB
OB
L�B
?�B
8�B
2�B
,qB
&2B
5B
]B
�B
�B

#B	��B	��B	�FB	�UB	�,B	�-B	�B	�B	�?B	��B	̳B	�#B	żB	�BB	��B	�B	��B	�OB	�	B	�B	�1B	��B	�mB	�jB	�B	}�B	{�B	zB	shB	`BB	VB	R B	NB	J#B	C{B	@iB	=qB	:�B	4B	0�B	/�B	-�B	,�B	*�B	'�B	$�B	!�B	pB	�B	�B	$B	B	�B	B		B	gB	�B	�B	�B�HB�0B�B�hB�vB��B�]B�2B�TB�bB�B��B��BӏBѷB�6B�)B��B��B�GB��B�B��B��B��B��B��B��B��B��B�CB�qB��B�_B��B�9B�2B�{B��B��B��B�XB�B�?B��B�'B�OB}VBz�Bv�Bp�Bn�Bk�Bi�Be,Bc:Ba�B_VB[	BV�BS[BR:BO�BOBBM�BLdBLJBLJBLJBKDBKDBKBJ=BJXBI�BH�BF�BDgBC�BC{BB'B?�B>BB>B>B=<B;�B7�B2�B1�B1�B1�B0�B/�B/iB.�B.�B.cB-]B.IB-B,�B,�B+QB)yB)yB)yB)_B)_B)yB*�B,�B-�B/OB1vB3�B3�B4B4TB5%B5ZB6�B8lB:B:�B;�B;�B;�B<�B<�B=<B<�B>(B@BB�BB�BB�BCBDBD�BE�BF�BHBIBK�BP�BR�BS&BVmBWYBWYBXyBYeBYBZkBZQB[WB[qB\]B\xB\]B\xB\�B]B_�Bc�Bh>BkBlBmBm�Bp�Bv+BxBy$Bz*BzBz*B{0B{B|6B|B|PB}qBHBcB��B��B��B��B�tB��B��B��B��B��B��B��B��B��B�?B�B��B��B�B�!B�'B�B�B�B�B� B�@B�FB�mB�kB�]B�wB�iB�[B�|B�aB��B�B�	B�B��B��B��B��B��B�-BǮB��B�NB�[B�{B�yB�qB�IB�dBބBߊBߊB�B�B��B��B�B��B�B�3B�FB�DB�B�cB	[B	gB	�B	�B		�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	!B	#B	$B	%B	%B	%B	%FB	'RB	*eB	-CB	.cB	0UB	0UB	1vB	2�B	5�B	7�B	7�B	9�B	;B	<�B	=�B	?�B	B�B	E�B	G�B	G�B	IB	J#B	K^B	QhB	X+B	XEB	YeB	ZQB	\]B	]dB	]dB	]dB	]�B	b�B	e�B	g�B	lB	p�B	r�B	s�B	t�B	u�B	v�B	v�B	v�B	xB	y	B	{B	|B	}"B	~(B	.B	.B	� B	�[B	�aB	�YB	�_B	�fB	�lB	�lB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�&B	�B	�8B	�*B	�KB	�CB	�OB	�UB	�UB	�;B	�AB	�AB	�[B	�[B	�[B	�[B	�aB	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	żB	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	� B	�B	�TB	�B	�B	�2B	�9B	�9B	�?B	�?B	�EB	�eB	�QB	�WB	�]B	�]B	�]B	�dB	�dB	�jB	�jB	�pB	�pB	ߊB	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�%B	�LB	��B	�B	�B	�B	�B	�B	�<B	�BB
 OB
;B
AB
GB
aB
gB
mB
?B
�B
�B
	�B

�B
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
 �B
 �B
 �B
 �B
 �B
!�B
#B
# B
# B
$&B
%�B
'B
'B
'B
'B
($B
($B
(XB
)DB
*KB
+B
+6B
,WB
,WB
-)B
./B
.IB
./B
.IB
.IB
/OB
.cB
/OB
/5B
/OB
0UB
0UB
0UB
1[B
1[B
1AB
1AB
1[B
1[B
1vB
2aB
2aB
2aB
3hB
33B
3MB
4TB
4TB
4TB
4TB
4TB
4nB
5ZB
5tB
5tB
5tB
5�B
6�B
7�B
9�B
:�B
:�B
;�B
=�B
>�B
>�B
>�B
?�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
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
H�B
J	B
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
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
MB
NB
NB
OB
OB
OB
OB
O�B
O�B
O�B
PB
PB
PB
P.B
Q B
Q B
RB
R B
R B
R B
R B
SB
R�B
SB
R�B
S&B
S&B
T,B
T,B
S�B
T,B
T,B
TFB
UMB
V9B
VB
VB
V9B
V9B
WYB
WYB
X+B
XEB
XEB
YeB
Y1B
ZkB
[WB
[WB
[WB
[=B
[=B
\CB
\CB
\)B
\CB
\)B
\CB
\CB
\CB
\)B
\]B
\]B
]IB
]dB
]dB
^jB
^jB
^5B
^OB
^OB
^OB
^jB
^jB
_pB
_pB
_pB
_�B
`vB
`\B
`vB
`\B
abB
`vB
`vB
abB
a|B
a|B
a|B
b�B
b�B
b�B
b�B
c�B
d�B
d�B
dtB
d�B
d�B
e�B
ezB
ezB
ezB
e�B
f�B
f�B
g�B
g�B
g�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
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
l�B
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
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
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
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.17(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201804200032512018042000325120180420003251202211182134192022111821341920221118213419201806041923402018060419234020180604192340  JA  ARFMdecpA19c                                                                20180409003526  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180408153621  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180408153623  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180408153623  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180408153624  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180408153624  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180408153624  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180408153624  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180408153624  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180408153624                      G�O�G�O�G�O�                JA  ARUP                                                                        20180408155728                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180408153300  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20180419153251  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180419153251  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604102340  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171535                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123419  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                