CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:16Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170916  20220204114420  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               ^A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @�ӝ{Bm�1   @�Ӟ @6�hr�!�c�M���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ^A   B   B   @�ff@�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B ��B'��B0  B8  B@  BH  BP  BXffB`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)  D)�fD*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3�fD4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Dn��Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�D��D�UqD��D��qD��D�d)D���D�� D��D�^D���D�θD�
D�QHDڑ�D��D��D�a�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@��\@��\AG�A=G�A[�A}G�A���A���A���A���AΣ�A�p�A��A���BQ�BQ�BQ�B �B&�B/Q�B7Q�B?Q�BGQ�BOQ�BW�RB_�RBgQ�BoQ�BwQ�BQ�B���B���B���B���B���B���B��)B���B���B���B���B���B���B���B���B���Bè�BǨ�B˨�BϨ�BӨ�Bר�Bۨ�B�u�B��B��B��B��B��B���B���B���C�{C�{C�{C�{C	�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C!�{C#�{C%�{C'�{C)�{C+�{C-�{C/�C1�{C3�{C5�{C7�{C9�{C;�{C=�{C?�{CA�{CC�{CE�{CG�{CI�{CK�{CM�{CO�{CQ�{CS�{CU�{CW�{CY�{C[�{C]�{C_�{Ca�{Cc�{Ce�{Cg�{Ci�{Ck�{Cm�{Co�{Cq�{Cs�{Cu�{Cw�{Cy�{C{�{C}�{C�{C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��pC��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=D uD �DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D	uD	�D
uD
�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD��D{�D�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D uD �D!uD!�D"uD"�D#uD#�D$uD$�D%uD%�D&{�D&�D'uD'�D(uD(�D){�D)�D*uD*�D+uD+�D,uD,�D-uD-�D.uD.�D/uD/�D0uD0�D1uD1�D2uD2�D3{�D3�D4uD4�D5uD5�D6uD6�D7uD7�D8uD8�D9uD9�D:uD:�D;uD;�D<uD<�D=uD=�D>uD>�D?uD?�D@uD@�DAuDA�DBuDB�DCuDC�DDuDD�DEuDE�DFuDF�DGuDG�DHuDH�DIuDI�DJuDJ�DKuDK�DLuDL�DMuDM�DNuDN�DOuDO�DPuDP�DQuDQ�DRuDR�DSuDS�DTuDT�DUuDU�DVuDV�DWuDW�DXuDX�DYuDY�DZuDZ�D[uD[�D\uD\�D]uD]�D^uD^�D_uD_�D`uD`�DauDa�DbuDb�DcuDc�DduDd�DeuDe�DfuDf�DguDg�DhuDh�DiuDi�DjuDj�DkuDk�DluDl�DmuDm�DnuDn�DouDo�DpuDp�DquDq�DruDr�DsuDs�DtuDt��Dy�3D�D�P D���D�� D�D�^�D��\D�ҏD�3D�X�D��D��GD��D�K�Dڌ{D��D�pD�\{D�D�˅111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aڰ!Aڴ9Aڴ9Aڴ9A�AړuA�M�A�-A�$�A� �A��A�bA�JA�A���A��#A١�A�^5A��
A�n�A�ZA�$�AĮA�A�t�A�;dA�A���A��A���A��A��;A�~�A�ĜA�n�A��;A�oA�ZA��A��!A�ȴA�A��hA�;dA���A���A��FA��A�7LA�bA�9XA�
=A���A�hsA��mA��yA�G�A���A��RA��uA��yA�r�A���A�9XA�jA�K�A�hsA�ƨA�A�A�Q�A�M�A�ȴA���A�n�A���A��A���A�VA��A�z�A���A�33A�  A��
A�~�A���A�bA�XA��A�$�A�A��9A���A��yA�1'A��A���A�O�A�l�A�
=A�ffA�^5A�ƨA���A���A�7LA�&�A}"�Ay\)Aw�#Av(�AtȴAr��Ap��An�Ak�^Aj�Aj�Ah�!Ag�Ae;dAb��Aa�hA`�+A_7LA]33AZ�AYoAW��AV�AV��AU�AT�RAS�hAR(�AO�mAN��AM��ALjAK�AJA�AIAH��AFffAEp�AEXAD�jAB��AA�AA�
A@~�A>��A>��A>A=�A<VA;hsA;�A:~�A9�hA8�A8n�A8  A7�FA7oA6ffA5�wA4�A3|�A2��A1�A1A/��A. �A,��A+�^A*�A(�jA&�RA%�hA$$�A#S�A"1A�PAbNAAM�AƨAC�A(�A��A�A�A�AhsAZA�A=qAl�A�`A�AĜA �A|�A��A�A�A
�+A	�Az�AffAVA�7A�TAS�A~�At�A�DA�hA E�@��@��h@��@��@�{@�\)@�+@�x�@�+@���@���@�`B@�u@�A�@�+@��#@�I�@�R@�^@�9X@��@ݑh@ܴ9@���@ڸR@���@�I�@���@Ԭ@�1@�t�@�@���@�A�@���@��`@���@�(�@�dZ@ʰ!@Ɂ@� �@Ǯ@Ƨ�@�hs@��@�G�@�7L@�9X@���@�Ĝ@�  @�|�@�33@��@��!@�v�@���@�Q�@�S�@���@���@�G�@�j@�(�@���@�;d@��@�=q@���@��@��`@�Q�@�33@�ȴ@�/@�bN@�  @�\)@�v�@��h@�&�@��D@��@��@�ff@��h@��@���@�Ĝ@��9@��D@�Q�@��F@�+@��@�v�@���@���@�I�@�1@�  @��@���@��@��@�E�@�@���@��@��\@�V@�M�@��#@���@��T@���@���@�`B@���@�  @�33@���@���@�bN@�S�@�=q@���@�9X@�j@��@��F@�1@�j@���@�  @�o@���@�^5@�V@�n�@��!@�@���@�^5@�x�@�`B@�z�@��@�S�@���@�E�@�^5@�^5@��7@�O�@�`B@�7L@���@��D@�j@� �@��@��;@�A�@�Q�@�I�@���@��@���@�Ĝ@���@�1'@���@�l�@�33@��R@�=q@��^@��@�G�@���@��u@�(�@���@�l�@�S�@�|�@��F@�l�@���@��R@��+@�E�@��@��T@��@���@��@��#@�x�@���@��D@���@�&�@�Ĝ@�9X@�b@�ƨ@���@���@�;d@�33@��@���@��!@�^5@�-@��@��^@���@��7@�X@��@��@��@��`@�Ĝ@�z�@�Q�@�A�@�  @��
@���@�C�@�
=@���@�~�@�M�@��@��-@��h@�O�@�7L@�&�@�%@��/@��@�1'@��m@��@�t�@�l�@�l�@�\)@�S�@�t�@�l�@�;d@�ȴ@���@�ff@�@���@��h@�?}@�%@���@���@~�L@z4@s��@k��@af�@Y7L@L��@F�M@?H�@;E9@47@/,�@*kQ@$ی@��@qv@��@n/@t�@v`@o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Aڰ!Aڴ9Aڴ9Aڴ9A�AړuA�M�A�-A�$�A� �A��A�bA�JA�A���A��#A١�A�^5A��
A�n�A�ZA�$�AĮA�A�t�A�;dA�A���A��A���A��A��;A�~�A�ĜA�n�A��;A�oA�ZA��A��!A�ȴA�A��hA�;dA���A���A��FA��A�7LA�bA�9XA�
=A���A�hsA��mA��yA�G�A���A��RA��uA��yA�r�A���A�9XA�jA�K�A�hsA�ƨA�A�A�Q�A�M�A�ȴA���A�n�A���A��A���A�VA��A�z�A���A�33A�  A��
A�~�A���A�bA�XA��A�$�A�A��9A���A��yA�1'A��A���A�O�A�l�A�
=A�ffA�^5A�ƨA���A���A�7LA�&�A}"�Ay\)Aw�#Av(�AtȴAr��Ap��An�Ak�^Aj�Aj�Ah�!Ag�Ae;dAb��Aa�hA`�+A_7LA]33AZ�AYoAW��AV�AV��AU�AT�RAS�hAR(�AO�mAN��AM��ALjAK�AJA�AIAH��AFffAEp�AEXAD�jAB��AA�AA�
A@~�A>��A>��A>A=�A<VA;hsA;�A:~�A9�hA8�A8n�A8  A7�FA7oA6ffA5�wA4�A3|�A2��A1�A1A/��A. �A,��A+�^A*�A(�jA&�RA%�hA$$�A#S�A"1A�PAbNAAM�AƨAC�A(�A��A�A�A�AhsAZA�A=qAl�A�`A�AĜA �A|�A��A�A�A
�+A	�Az�AffAVA�7A�TAS�A~�At�A�DA�hA E�@��@��h@��@��@�{@�\)@�+@�x�@�+@���@���@�`B@�u@�A�@�+@��#@�I�@�R@�^@�9X@��@ݑh@ܴ9@���@ڸR@���@�I�@���@Ԭ@�1@�t�@�@���@�A�@���@��`@���@�(�@�dZ@ʰ!@Ɂ@� �@Ǯ@Ƨ�@�hs@��@�G�@�7L@�9X@���@�Ĝ@�  @�|�@�33@��@��!@�v�@���@�Q�@�S�@���@���@�G�@�j@�(�@���@�;d@��@�=q@���@��@��`@�Q�@�33@�ȴ@�/@�bN@�  @�\)@�v�@��h@�&�@��D@��@��@�ff@��h@��@���@�Ĝ@��9@��D@�Q�@��F@�+@��@�v�@���@���@�I�@�1@�  @��@���@��@��@�E�@�@���@��@��\@�V@�M�@��#@���@��T@���@���@�`B@���@�  @�33@���@���@�bN@�S�@�=q@���@�9X@�j@��@��F@�1@�j@���@�  @�o@���@�^5@�V@�n�@��!@�@���@�^5@�x�@�`B@�z�@��@�S�@���@�E�@�^5@�^5@��7@�O�@�`B@�7L@���@��D@�j@� �@��@��;@�A�@�Q�@�I�@���@��@���@�Ĝ@���@�1'@���@�l�@�33@��R@�=q@��^@��@�G�@���@��u@�(�@���@�l�@�S�@�|�@��F@�l�@���@��R@��+@�E�@��@��T@��@���@��@��#@�x�@���@��D@���@�&�@�Ĝ@�9X@�b@�ƨ@���@���@�;d@�33@��@���@��!@�^5@�-@��@��^@���@��7@�X@��@��@��@��`@�Ĝ@�z�@�Q�@�A�@�  @��
@���@�C�@�
=@���@�~�@�M�@��@��-@��h@�O�@�7L@�&�@�%@��/@��@�1'@��m@��@�t�@�l�@�l�@�\)@�S�@�t�@�l�@�;d@�ȴ@���@�ff@�@���@��h@�?}@�%@���G�O�@~�L@z4@s��@k��@af�@Y7L@L��@F�M@?H�@;E9@47@/,�@*kQ@$ی@��@qv@��@n/@t�@v`@o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB^5B]/B]/B\)B]/BZBXBW
BW
BW
BW
BW
BXBXBYB\)B`BB`BB�oB��B�BȴB��B�yB��B-BB�BL�BbNBt�Bk�Bu�Bo�BP�B!�B.BO�BP�BT�B^5BbNBaHBS�B<jB;dB7LB,BR�Bx�B~�B~�Bn�BR�BF�B@�BaHBk�BcTB[#BJ�BE�BD�B?}B=qB2-B0!B(�B �B�B!�B+B��B�sB��BB�BɺB�ZB�`B�NB�#B��BɺBǮB��B�!B��B�JBr�B]/BK�BQ�BYBK�B8RB!�B	7B
�B
�HB
�B
�B
�B
��B
��B
��B
��B
�7B
m�B
N�B
@�B
6FB
.B
�B
DB	��B	�fB	�BB	�B	��B	ƨB	�dB	�B	��B	��B	�\B	�7B	x�B	o�B	hsB	_;B	]/B	ZB	P�B	F�B	@�B	7LB	33B	49B	:^B	5?B	0!B	.B	,B	"�B	�B	�B	�B	\B	DB		7B	+B��B��B��B��B�B�B�B�yB�`B�NB�;B�/B�#B�B�B��B��B��BŢB��B�}B�XB�9B�B��B��B��B��B��B�bB�PB�=B�B~�Bz�Bx�Bv�Bt�Bq�Bo�Bk�BiyBgmBdZBcTB_;B\)B\)BZBYBYBW
BT�BS�BN�BM�BJ�BJ�BF�BF�BE�BE�BB�B@�B?}B=qB<jB;dB8RB6FB49B33B33B33B49B2-B33B2-B/B1'B-B+B+B+B,B-B-B-B/B/B/B/B0!B/B0!B2-B2-B7LB9XB:^B:^B;dB<jB<jB:^B9XB9XB=qBC�BF�BE�BF�BJ�BJ�BJ�BL�BM�BN�BP�BR�BS�BXBYB[#B]/B]/B_;BbNBbNBdZBjBk�Bl�Bn�Bp�Bq�Br�Bt�Bw�Bx�Bx�Bz�B�B�B�+B�=B�DB�VB�hB�{B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�'B�-B�9B�RB�jB�qB�qB�wB�}B��BƨBǮB��B��B��B�5B�`B�sB�B�B�B�B�B��B��B��B��B	B	1B	+B	1B		7B	uB	�B	 �B	#�B	$�B	+B	.B	2-B	33B	0!B	.B	-B	/B	2-B	7LB	<jB	F�B	I�B	J�B	K�B	L�B	Q�B	P�B	R�B	R�B	VB	YB	YB	ZB	]/B	`BB	`BB	bNB	bNB	dZB	jB	l�B	p�B	q�B	r�B	v�B	|�B	�B	�%B	�%B	�7B	�PB	�VB	�VB	�PB	�JB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�9B	�?B	�FB	�LB	�LB	�LB	�RB	�jB	�qB	�qB	�qB	�wB	�}B	��B	ĜB	ŢB	ŢB	ŢB	ŢB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�)B	�/B	�5B	�;B	�BB	�HB	�HB	�NB	�NB	�NB	�TB	�TB	�TB	�ZB	�ZB	�`B	�fB	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
gB
.B
aB
_B
%zB
/�B
?�B
AUB
F�B
J�B
P�B
V�B
]/B
b�B
eB
jB
o B
s�B
w�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BU�BT{BT{BSuBT{BQjBO]BNWBNWBNWBNWBNWBO]BO]BPdBSvBW�BW�B��B�9B�WB��B�@B��B�B$QB9�BDBY�Bk�Bb�BmBf�BH'BB%YBG"BH(BLABUwBY�BX�BK;B3�B2�B.�B#OBJ6BpBv<Bv<Be�BJ7B=�B7�BX�Bb�BZ�BRhBBB<�B;�B6�B4�B)vB'jB @BB�BB�xB�#B��B�*B��B�hB�B۪BܰBٞB�tB�1B�B�B��B�uB�B��Bj	BT�BC#BIHBPrBC#B/�B*B �B
�B
جB
�B
�iB
�wB
�4B
�:B
�RB
�:B
��B
d�B
FIB
7�B
-�B
%�B
B
�B	�^B	��B	׻B	БB	�_B	�#B	��B	�B	�6B	�B	��B	��B	pWB	g B	_�B	V�B	T�B	Q�B	HjB	>.B	8	B	.�B	*�B	+�B	1�B	,�B	'�B	%�B	#�B	[B	0B	$B	B	�B	�B	 �B��B��B�{B�hB�]B�DB�&B�B�B��B��B��BԿBҳBѭB͕BˉB�dB�SB�4B�B�B��B��B��B��B�xB�[B�1B�B��B��B��Bz�Bv�Br{BpoBncBlWBiEBg9Bc!BaB_	B[�BZ�BV�BS�BS�BQ�BP�BP�BN�BL�BK�BFxBErBBaBBaB>HB>HB=BB=BB:0B8$B7B5B4B3B/�B-�B+�B*�B*�B*�B+�B)�B*�B)�B&�B(�B$�B"�B"�B"�B#�B$�B$�B$�B&�B&�B&�B&�B'�B&�B'�B)�B)�B.�B0�B2B2B3	B4B4B2B0�B0�B5B;;B>MB=HB>NBBfBBfBBfBDrBExBF~BH�BJ�BK�BO�BP�BR�BT�BT�BV�BY�BY�B[�Bb#Bc)Bd/Bf<BhHBiNBjTBl`BosBpyBpyBr�Bx�Bz�B~�B��B��B��B�B�B�#B�5B�BB�`B�mB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�#B�HB�NB�aB�mBʑB��B��B�B�B�/B�6B�<B�AB�YB�rB�lB�~B��B��B��B��B	 �B	B	;B	`B	rB	xB	"�B	%�B	)�B	*�B	'�B	%�B	$�B	&�B	)�B	.�B	4B	>@B	ARB	BYB	C_B	DeB	I�B	H}B	J�B	J�B	M�B	P�B	P�B	Q�B	T�B	W�B	W�B	Y�B	Y�B	[�B	bB	d"B	h:B	i@B	jFB	n_B	t�B	x�B	}�B	}�B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�@B	�LB	�RB	�^B	�jB	�pB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�	B	�B	�B	�-B	�3B	�3B	�3B	�3B	�9B	�?B	�?B	�EB	�RB	�^B	�^B	�pB	�vB	�vB	�vB	�}B	̏B	̏B	͕B	ΛB	ϠB	ЧB	ҳB	ӹB	ԿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�-B	�3B	�9B	�9B	�9B	�KB	�KB	�KG�O�B	�B	��B
�B
�B
�B
B
'B
7XB
8�B
>hB
BgB
H=B
N-B
T�B
ZB
\�B
b
B
f�B
k[B
o?B
r111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.17 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.005) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144202022020411442020220204114420  AO  ARCAADJP                                                                    20200619170916    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170916  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170916  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114420  IP                  G�O�G�O�G�O�                