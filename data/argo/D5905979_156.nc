CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T14:00:59Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619140059  20220204114426  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @�!ڤ�1   @�!m�D2@5�z�G��b�"��`B1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(�C*�C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD � D  D�fDfD�fD  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  Dy�D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.fD.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D���D�\)D��fD��qD�&D�[�D���D���D��D�M�D�2�D��D��D�W\DڎfD�ʏD�&D�[�D�
D�
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�(�@��\@��\AG�A=G�A]G�A}G�A���A���A���A���AΣ�Aޣ�A�p�A���BQ�BQ�BQ�BQ�B'Q�B/Q�B7Q�B?Q�BGQ�BOQ�BW�RB_Q�BgQ�BoQ�BwQ�BQ�B���B���B���B�u�B���B���B���B���B���B���B���B���B���B���B���B���Bè�BǨ�B˨�BϨ�BӨ�Bר�Bۨ�Bߨ�B��B��B��B��B��B���B���B���C�{C�{C�{C�{C	�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C!�{C#�{C%�{C'�C)�C+�{C-�{C/�{C1�{C3�{C5�{C7�{C9�{C;�{C=�{C?�{CA�{CC�{CE�{CG�{CI�{CK�{CM�{CO�{CQ�{CS�{CU�{CW�{CY�{C[�{C]�{C_�{Ca�{Cc�{Ce�{Cg�{Ci�{Ck�{Cm�{Co�{Cq�{Cs�{Cu�{Cw�{Cy�{C{�{C}�{C�{C��pC��=C��=C��pC��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��pC��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��pC��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��	D uD �D{�D��D{�D�DuD�DuD�DuD�DuD�DuD�DuD�D	uD	�D
uD
�DuD�DuD�DuD�DuD�DuD��DuD�DuD�DuD�Dn�D�DuD�DuD�Dn�D�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D uD �D!uD!�D"uD"�D#uD#�D$uD$�D%uD%�D&uD&�D'uD'�D(uD(�D)uD)�D*uD*�D+uD+�D,uD,�D-uD-��D.uD.�D/uD/�D0uD0�D1uD1�D2uD2�D3uD3�D4uD4�D5uD5�D6uD6�D7uD7�D8uD8�D9uD9�D:uD:�D;uD;�D<uD<�D=uD=�D>uD>�D?uD?�D@uD@�DAuDA�DBuDB�DCuDC�DDuDD�DEuDE�DFuDF�DGuDG�DHuDH�DIuDI�DJuDJ�DKuDK�DLuDL�DMuDM�DNuDN�DOuDO�DPuDP�DQuDQ�DRuDR�DSuDS�DTuDT�DUuDU�DVuDV�DWuDW�DXuDX�DYuDY�DZuDZ�D[uD[�D\uD\�D]uD]�D^uD^�D_uD_�D`uD`�DauDa�DbuDb�DcuDc�DduDd�DeuDe�DfuDf�DguDg�DhuDh�DiuDi�DjuDj�DkuDk�DluDl�DmuDm�DnuDn�DouDo�DpuDp�DquDq�DruDr�DsuDs�DtuDt�RDy��D��GD�V�D���D�� D� �D�VfD��\D�ۅD�D�HRD�-D��D�D�Q�Dڈ�D��D� �D�VD�D�y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A���A�A�%A�A�%A�%A�%A��Aĩ�AđhA�^5A��A¡�A�|�A��A�x�A��/A�l�A���A�(�A���A�9XA��A���A�ffA�O�A�VA���A�v�A�O�A�+A��/A��7A�/A��;A��hA�t�A�jA�XA��A�bA��A���A���A�|�A��A�1'A��A�A�z�A��\A�v�A��A���A�-A�1A��
A��uA�;dA��\A�VA���A�dZA�33A��!A�A�G�A�A��A�JA�^5A�ZA�O�A��wA��9A�O�A��A�\)A��A��uA�1A��A�ZA�ZA���A���A�E�A�bA��FA�$�A�O�A���A��TA�Q�A�oA�ƨA���A�;dA���A�S�A�JA���A��
A��A��A�{A�dZA���A��jA�-A��A��mA�5?A�=qA~��A}�7Az�+Ax-Au��As%Ap�uAn  AlbAi��AfbAc|�Abn�A`�!A]��A]G�A\�RAZ��AXQ�AV�AU�7ASt�AQ�hAP��AP1'AO��AO�AM��AK�AI\)AHZAF^5AC�^AB��A@�A>^5A<�\A:JA8M�A6��A5"�A2�+A1/A0n�A/�hA.bNA-��A,��A,�A*��A)�hA(bA%�wA#�A#G�A#"�A"�yA"r�A!�A ��A�;A��AdZA33A�yA�AAv�A�AjA�TA�HA�;A`BA�HAE�A�A�AjAA�A7LA��A��A�`A�#A��AffAE�A(�A�PA
AE�A�FA+AZAK�A�9A=qA�#A|�A�9AAA �R@��@�x�@��@�@�S�@�x�@��@� �@�@���@��H@�p�@�5?@�v�@�"�@�x�@��@��@�%@�
=@�ff@�$�@٩�@�?}@�I�@�K�@�@�$�@ԓu@��m@Ұ!@�`B@�7L@�b@���@�G�@��@���@��@�M�@���@�@ɉ7@�?}@���@�A�@��H@���@å�@�
=@�v�@�=q@�J@��T@��@�1'@��h@���@��;@�ȴ@�M�@�~�@��@��\@�G�@��@��H@���@�?}@�G�@�`B@�/@�I�@���@�t�@��\@���@�?}@���@�;d@�^5@�J@��^@��`@��w@��@�=q@��@���@�hs@��9@��@��@��R@�V@��@��-@���@�z�@�b@�|�@�"�@�@���@���@�J@��^@�hs@�V@��j@���@�bN@���@��m@���@�|�@�dZ@�dZ@�\)@�dZ@�|�@�t�@�
=@���@���@�~�@�V@�-@��@�p�@�O�@�7L@��`@���@���@��@�r�@�bN@�9X@�ƨ@�t�@��@���@��@���@�ȴ@��\@�-@��#@���@��7@�G�@��@��`@�Ĝ@�z�@�Q�@��@��F@�|�@�K�@�33@��@�~�@�@��-@��@�Ĝ@��@���@�t�@�\)@�+@�o@�
=@��H@���@�$�@��h@�G�@�V@���@���@���@��D@�1'@��;@��@�|�@�K�@�"�@�@�ȴ@���@�{@��#@���@���@��h@��7@�p�@�7L@��`@�z�@�A�@� �@�1@���@�K�@��@���@�~�@���@���@�@��-@���@��h@��@�`B@�&�@���@��/@��9@���@�r�@�Z@�(�@��F@�l�@�C�@�"�@���@���@��!@��\@�=q@�$�@�@�x�@�x�@�O�@��@���@��/@���@��9@�z�@�(�@�1@��m@��w@�|�@�;d@�"�@��@��M@{�@r�@j�@bȴ@\�D@U��@Nc @G�@A�#@<��@6�"@/��@)�H@$U2@��@��@@�x@�7@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A���A�A�%A�A�%A�%A�%A��Aĩ�AđhA�^5A��A¡�A�|�A��A�x�A��/A�l�A���A�(�A���A�9XA��A���A�ffA�O�A�VA���A�v�A�O�A�+A��/A��7A�/A��;A��hA�t�A�jA�XA��A�bA��A���A���A�|�A��A�1'A��A�A�z�A��\A�v�A��A���A�-A�1A��
A��uA�;dA��\A�VA���A�dZA�33A��!A�A�G�A�A��A�JA�^5A�ZA�O�A��wA��9A�O�A��A�\)A��A��uA�1A��A�ZA�ZA���A���A�E�A�bA��FA�$�A�O�A���A��TA�Q�A�oA�ƨA���A�;dA���A�S�A�JA���A��
A��A��A�{A�dZA���A��jA�-A��A��mA�5?A�=qA~��A}�7Az�+Ax-Au��As%Ap�uAn  AlbAi��AfbAc|�Abn�A`�!A]��A]G�A\�RAZ��AXQ�AV�AU�7ASt�AQ�hAP��AP1'AO��AO�AM��AK�AI\)AHZAF^5AC�^AB��A@�A>^5A<�\A:JA8M�A6��A5"�A2�+A1/A0n�A/�hA.bNA-��A,��A,�A*��A)�hA(bA%�wA#�A#G�A#"�A"�yA"r�A!�A ��A�;A��AdZA33A�yA�AAv�A�AjA�TA�HA�;A`BA�HAE�A�A�AjAA�A7LA��A��A�`A�#A��AffAE�A(�A�PA
AE�A�FA+AZAK�A�9A=qA�#A|�A�9AAA �R@��@�x�@��@�@�S�@�x�@��@� �@�@���@��H@�p�@�5?@�v�@�"�@�x�@��@��@�%@�
=@�ff@�$�@٩�@�?}@�I�@�K�@�@�$�@ԓu@��m@Ұ!@�`B@�7L@�b@���@�G�@��@���@��@�M�@���@�@ɉ7@�?}@���@�A�@��H@���@å�@�
=@�v�@�=q@�J@��T@��@�1'@��h@���@��;@�ȴ@�M�@�~�@��@��\@�G�@��@��H@���@�?}@�G�@�`B@�/@�I�@���@�t�@��\@���@�?}@���@�;d@�^5@�J@��^@��`@��w@��@�=q@��@���@�hs@��9@��@��@��R@�V@��@��-@���@�z�@�b@�|�@�"�@�@���@���@�J@��^@�hs@�V@��j@���@�bN@���@��m@���@�|�@�dZ@�dZ@�\)@�dZ@�|�@�t�@�
=@���@���@�~�@�V@�-@��@�p�@�O�@�7L@��`@���@���@��@�r�@�bN@�9X@�ƨ@�t�@��@���@��@���@�ȴ@��\@�-@��#@���@��7@�G�@��@��`@�Ĝ@�z�@�Q�@��@��F@�|�@�K�@�33@��@�~�@�@��-@��@�Ĝ@��@���@�t�@�\)@�+@�o@�
=@��H@���@�$�@��h@�G�@�V@���@���@���@��D@�1'@��;@��@�|�@�K�@�"�@�@�ȴ@���@�{@��#@���@���@��h@��7@�p�@�7L@��`@�z�@�A�@� �@�1@���@�K�@��@���@�~�@���@���@�@��-@���@��h@��@�`B@�&�@���@��/@��9@���@�r�@�Z@�(�@��F@�l�@�C�@�"�@���@���@��!@��\@�=q@�$�@�@�x�@�x�@�O�@��@���@��/@���@��9@�z�@�(�@�1@��m@��w@�|�@�;d@�"�G�O�@��M@{�@r�@j�@bȴ@\�D@U��@Nc @G�@A�#@<��@6�"@/��@)�H@$U2@��@��@@�x@�7@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
N�B
O�B
O�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
M�B
M�B
I�B
A�B
33B
$�B
�B
�B
�B
�B
!�B
%�B
)�B
/B
:^B
E�B
J�B
R�B
^5B
e`B
k�B
p�B
z�B
�1B
��B
��B
�jB
ĜB
ƨB
��B
�ZB
�B
�B
��BBoBVB#�B9XB@�BP�Be`Bm�B}�B�oB��B��B��B�B�-B��B��B�;B�fB�B��BBDBJB�B�B�B�B�B"�B�B�B�B{BVBVBJBJBB��B�mB�5B�B��B��B�LB��B��B�JB�=Bv�BbNBC�B5?B,B�BbBDB
��B
�TB
��B
�wB
�'B
��B
��B
�=B
x�B
bNB
XB
E�B
6FB
0!B
�B
B	�B	�/B	��B	�3B	��B	��B	z�B	gmB	^5B	VB	B�B	<jB	5?B	$�B	�B	B	  B��B�B�B�B�B�sB�ZB�B��B��BȴBĜB�}B�XB�-B�B��B��B��B��B�PB�%B�%B�B� B}�B�B�=B�Bz�Bx�Bp�BhsBe`Be`Be`Be`BgmBhsBffBe`BgmBgmBhsBl�Bl�Bk�B{�B�7B�+B�oB�hB�hB�VB�7B�B{�Bv�Bl�BgmBffBk�BiyBhsBiyBgmBgmBffBjBo�Bq�Bq�Bo�Bo�Bm�Bq�Bp�Bn�Bm�BiyBiyBl�Bn�Bm�Bm�Bn�Bk�BjBiyBiyBiyBjBhsBbNBaHB^5BVB[#B\)B`BB_;BhsBffBiyBk�Bm�Bp�Bq�Bo�Bn�Bo�Bp�Bn�Bm�BiyBgmBgmBe`BdZBdZBjBiyBjBl�Bn�Bq�Bt�Bu�Bw�Bz�B}�B~�B~�B~�B}�B}�B|�B}�B|�B}�B~�B�B�B�B�1B�PB�hB�oB�hB�hB�oB��B��B��B��B��B��B��B��B��B�B�B�!B�'B�-B�3B�FB�jB��BÖBĜBŢBǮB��B��B��B�B�B�B�#B�;B�TB�sB�B�B�B�B�B��B��B��B��B	  B	B	B	+B	1B	
=B	JB	VB	VB	\B	hB	�B	�B	�B	�B	!�B	$�B	%�B	'�B	)�B	/B	0!B	1'B	49B	49B	5?B	7LB	8RB	9XB	:^B	@�B	B�B	E�B	F�B	G�B	H�B	H�B	K�B	P�B	T�B	W
B	XB	[#B	_;B	_;B	aHB	dZB	ffB	gmB	k�B	m�B	p�B	p�B	r�B	w�B	z�B	{�B	}�B	� B	�B	�B	�+B	�+B	�7B	�DB	�JB	�PB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�9B	�?B	�FB	�RB	�XB	�^B	�dB	�dB	�jB	�qB	�wB	�wB	�}B	��B	��B	��B	��B	ÖB	ǮB	ǮB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�/B	�5B	�5B	�5B	�;B	�;B	�BB	�BB	�HB	�NB	�ZB	�ZB	�`B	�fB	�mB	�yB	�B	�B	�B
-B
�B
�B
&�B
.�B
7fB
B�B
GEB
M�B
QhB
X+B
\�B
bNB
f�B
j�B
o�B
t�B
x�B
{�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
? B
@B
@B
? B
@B
@B
@B
@B
@B
@B
@B
@B
@B
@B
@B
@B
@B
@B
@B
@B
@B
@B
@B
@B
=�B
=�B
9�B
1�B
#_B
B
�B
�B
�B
�B
�B
B
+B
IB
*�B
5�B
:�B
CB
N^B
U�B
[�B
`�B
kB
xUB
��B
�B
��B
��B
��B
��B
�tB
ݪB
��B
��B
�B�B
�kB�B)hB0�B@�BUkB]�Bm�B�uB��B��B��B�B�/B��B��B�8B�cBہB��B�B�=B�CB�B
�B�B�B�B�B�B
�B{BvB�RB�SB�GB�GB�B��B�pB�:B�"B��B��B�WB�B��B|[BzNBf�BReB3�B%\B'B�B �B
�gB
�B
�}B
�B
��B
�WB
�	B
��B
zrB
iB
R�B
HLB
5�B
&�B
 cB

�B	�UB	��B	�}B	�B	��B	�?B	��B	k=B	W�B	N�B	FfB	2�B	,�B	%�B	FB	�B��B�nB�PB�B�B�B��B��B��BʑB�nB�>B�,B�B��B��B��B��B�VB�8B�,B�B}�Bv�Bv�Bs�Bp�BnzBq�Bz�Bu�BkhBi\Ba-BX�BU�BU�BU�BU�BW�BX�BV�BU�BW�BW�BX�B]B]B\BlqBy�Bw�B��B��B��B~�By�Bu�BlrBgUB]BW�BV�B\BZBYBZBW�BW�BV�B[B`-Bb9Bb:B`.B`.B^"Bb:Ba5B_)B^"BZBZB]B_*B^#B^#B_*B\B[BZBZBZB[BYBR�BQ�BN�BF�BK�BL�BP�BO�BY
BV�BZB\B^(Ba:Bb@B`5B_/B`5Ba;B_/B^(BZBXBXBU�BT�BT�B[BZB[B]$B_0BbBBeTBf[BhgBkxBn�Bo�Bo�Bo�Bn�Bn�Bm�Bn�Bm�Bn�Bo�Br�Br�Bs�Bx�B}�B��B�B��B��B�B�B�B�)B�6B�HB�ZB�`B�xB��B��B��B��B��B��B��B��B��B�B�(B�.B�4B�@B�SB�pBĉBƕBȠBɧB˳B��B��B�B�B�B�&B�,B�>B�QB�\B�nB�{B��B�B�B��B��B��B��B��B��B��B	�B	B		B	*B	AB	TB	eB	kB	xB	�B	�B	 �B	!�B	$�B	$�B	%�B	'�B	(�B	)�B	*�B	1B	3B	6'B	7-B	83B	99B	99B	<KB	AiB	E�B	G�B	H�B	K�B	O�B	O�B	Q�B	T�B	V�B	W�B	\B	^B	a$B	a$B	c0B	hOB	k`B	lfB	nsB	pB	s�B	u�B	w�B	w�B	y�B	{�B	|�B	}�B	�B	��B	��B	�B	�
B	�B	�(B	�.B	�.B	�LB	�RB	�dB	�qB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�&B	�&B	�&B	�,B	�,B	�,B	�2B	�9B	�DB	�JB	�\B	�cB	�cB	�iB	�iB	�oB	�uB	�zB	ǀB	ȆB	ȆB	ɍB	ɍB	ɍB	˙B	˙B	ͥB	ΫB	ΫB	ΫB	ϱB	ϱB	иB	иB	ѽB	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B	�B
 6B
.B
YB
;B
'�B
3fB
7�B
>ZB
A�B
H�B
L�B
R�B
W;B
[:B
`#B
eBB
i&B
lRB
p111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.17 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.015(+/-0.005) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144272022020411442720220204114427  AO  ARCAADJP                                                                    20200619140059    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619140059  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619140059  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114427  IP                  G�O�G�O�G�O�                