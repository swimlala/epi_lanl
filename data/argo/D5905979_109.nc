CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:18Z creation      
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
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �0   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �D   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �T   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �X   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �\   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20200619170918  20220204114422  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               mA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @��^$�G�1   @��^�o�@5�p��
=�cC��%1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    mA   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bg��Bp  Bx  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B���B���B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(�fD)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE�fDF  DFy�DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dj��Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy��D�)D�^fD���D��D�${D�_
D��=D��D�'\D�_�D��qD��
D�
D�X DژRD��)D�'�D�a�D�{D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�(�@��\@��\AG�A=G�A]G�A}G�A���A���A���A���AΣ�Aޣ�A��A���BQ�BQ�BQ�BQ�B'Q�B/Q�B7Q�B?Q�BG�RBOQ�BWQ�B_Q�Bf�BoQ�BwQ�BQ�B���B��)B���B�u�B���B���B���B���B���B�B�B�B�B���B���B�u�B�u�B���Bè�BǨ�B˨�BϨ�BӨ�Bר�Bۨ�Bߨ�B��B��B��B��B��B���B���B���C�{C�{C�{C�{C	�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C!�{C#�{C%�{C'�{C)�{C+�{C-�{C/�{C1�{C3�{C5�{C7�{C9�{C;�{C=�{C?�{CA�{CC�{CE�{CG�{CI�{CK�{CM�{CO�{CQ�{CS�{CU�CW�{CY�{C[�{C]�{C_�{Ca�{Cc�{Ce�{Cg�{Ci�{Ck�Cm�{Co�{Cq�{Cs�{Cu�{Cw�{Cy�{C{�{C}�{C�{C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��
C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=D uD �DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D	uD	�D
uD
�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D uD �D!uD!�D"uD"�D#uD#�D$uD$�D%uD%�D&uD&�D'uD'�D({�D(�D)uD)�D*uD*�D+uD+�D,uD,�D-uD-�D.uD.�D/uD/�D0uD0�D1uD1�D2uD2�D3uD3�D4uD4�D5uD5�D6uD6�D7uD7�D8uD8�D9uD9�D:uD:�D;uD;�D<uD<�D=uD=�D>uD>�D?uD?�D@uD@�DAuDA�DBuDB�DCuDC�DDuDD�DE{�DE�DFn�DF�DGuDG�DHuDH�DIuDI�DJuDJ�DKuDK�DLuDL�DMuDM�DNuDN�DOuDO�DPuDP�DQuDQ�DRuDR�DSuDS�DTuDT�DUuDU�DVuDV�DWuDW�DXuDX�DYuDY�DZuDZ�D[uD[�D\uD\�D]uD]�D^uD^�D_uD_�D`uD`�DauDa�DbuDb�DcuDc�DduDd�DeuDe�DfuDf�DguDg�DhuDh�DiuDi�DjuDj�DkuDk�DluDl�DmuDm�DnuDn�DouDo�DpuDp�DquDq�DruDr�DsuDs�DthRDy��D��D�X�D��\D���D�
D�Y�D���D�ǮD�!�D�Z=D�� D�əD��D�R�Dڒ�D�޸D�"=D�\)D�
D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�7LA�;dA�=qA�=qA�=qA�=qA�?}A�C�A�C�A�A�A�C�A�I�A�M�A�E�A�O�A�ZA�hsA�^5A�A�A�5?A�(�A��A�oA�oA�
=A�A���A���A��HA�7LA�ȴA�9XAЧ�AΧ�A��
A�O�A���Aș�A�/A�%A��A��A¸RA��HA�bA�O�A��RA��TA��A���A�M�A�33A�K�A��A�;dA�1'A���A�VA���A�JA��DA��A�l�A�ZA�dZA���A��A�&�A�I�A�M�A�%A�jA��A��A�jA��A�1'A�A��hA� �A���A���A�VA��wA�G�A��A�VA���A���A���A���A�(�A�v�A���A��PA��A�M�A�l�A���A�^5A�r�A���A���A��^A�E�A|jAzJAx�yAw�TAt��Ar$�ApbNAo/Am�Ak�AhA�Af��Ae�PAd�Aa�A`�A`�DA[�mAY�
AX�jAW�AS��AP�/AO��AL5?AKS�AI��AH�AG%AFz�AEdZAC�^AB�9AA��A@�jA@  A?|�A>�A=p�A;p�A9��A8�HA7�A6VA5��A4ffA3S�A2��A2z�A1��A1�7A1\)A0��A/�FA.�uA-�mA-�A,��A+�^A*ȴA)G�A'p�A%"�A#��A#;dA"=qA!��A!C�A��A��A�AI�A%A^5A1A�wA��AK�A��A��A�jAA�AI�A�A��AĜAE�A�FA�+AƨA��A�;A��Ap�A33A$�A
�A
ZA
-A	�A
  A	��A��A$�A�jA�;A%A�PA��A�AC�A �uA (�@��H@��/@��@� �@�S�@�~�@��@��H@�A�@�\@�|�@��@��@�t�@�K�@��m@�?}@�v�@��@��@��m@��@ܴ9@۶F@���@�v�@؋D@�=q@�1'@�
=@��@�r�@�o@́@�7L@�t�@���@�-@�Q�@�o@�n�@���@š�@�bN@�ff@�@�&�@���@��y@�V@�&�@�Z@���@���@���@��j@�9X@��@�;d@��@�p�@��@�bN@���@�@�O�@��@�r�@��@��@�~�@��@���@���@���@�33@�o@�"�@�+@�"�@�ff@��@�hs@�r�@�Z@�1@�S�@�+@��@��H@��@��!@�{@��@���@��/@�r�@�ƨ@�|�@�"�@��H@��@�-@��-@�7L@�j@��F@�"�@���@�-@�x�@�X@�Ĝ@���@��@���@��@���@�^5@�{@�@�&�@��j@��@��9@�A�@��w@�C�@��H@�{@�$�@���@���@��@���@�bN@�9X@� �@��@��
@�ƨ@��@���@��P@�\)@�33@�ȴ@���@�~�@�E�@�=q@�-@��@���@�?}@�X@�/@�/@���@�(�@��m@��;@�ƨ@�t�@�C�@�33@�o@�\)@��@��@��@���@���@��!@��\@�ff@�E�@�@��@��#@���@��^@��h@��7@�x�@�X@�&�@�%@�Ĝ@��u@�r�@�1'@��@��@�  @�ƨ@��F@��w@�ƨ@��F@��w@��P@�\)@�dZ@�"�@���@��!@�v�@�V@�-@�J@���@��@���@�x�@�hs@�X@�%@���@�1'@��m@��
@��w@���@�|�@�;d@�+@��@��@���@��+@�ff@�M�@��@��T@���@�hs@�O�@�G�@�&�@�V@��j@�r�@�I�@�I�@�1'@�b@��@��@��@��m@��;@��
@�ƨ@��F@���@�|�@�33@�@��H@��R@�J@���@���@���@�`B@�&�@���@��@}�N@t��@nC�@f�@`ی@Y`B@RkQ@I�@C� @<tT@7��@2�@+P�@&�8@ C-@��@@�@U�@�@
�@3�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�7LA�;dA�=qA�=qA�=qA�=qA�?}A�C�A�C�A�A�A�C�A�I�A�M�A�E�A�O�A�ZA�hsA�^5A�A�A�5?A�(�A��A�oA�oA�
=A�A���A���A��HA�7LA�ȴA�9XAЧ�AΧ�A��
A�O�A���Aș�A�/A�%A��A��A¸RA��HA�bA�O�A��RA��TA��A���A�M�A�33A�K�A��A�;dA�1'A���A�VA���A�JA��DA��A�l�A�ZA�dZA���A��A�&�A�I�A�M�A�%A�jA��A��A�jA��A�1'A�A��hA� �A���A���A�VA��wA�G�A��A�VA���A���A���A���A�(�A�v�A���A��PA��A�M�A�l�A���A�^5A�r�A���A���A��^A�E�A|jAzJAx�yAw�TAt��Ar$�ApbNAo/Am�Ak�AhA�Af��Ae�PAd�Aa�A`�A`�DA[�mAY�
AX�jAW�AS��AP�/AO��AL5?AKS�AI��AH�AG%AFz�AEdZAC�^AB�9AA��A@�jA@  A?|�A>�A=p�A;p�A9��A8�HA7�A6VA5��A4ffA3S�A2��A2z�A1��A1�7A1\)A0��A/�FA.�uA-�mA-�A,��A+�^A*ȴA)G�A'p�A%"�A#��A#;dA"=qA!��A!C�A��A��A�AI�A%A^5A1A�wA��AK�A��A��A�jAA�AI�A�A��AĜAE�A�FA�+AƨA��A�;A��Ap�A33A$�A
�A
ZA
-A	�A
  A	��A��A$�A�jA�;A%A�PA��A�AC�A �uA (�@��H@��/@��@� �@�S�@�~�@��@��H@�A�@�\@�|�@��@��@�t�@�K�@��m@�?}@�v�@��@��@��m@��@ܴ9@۶F@���@�v�@؋D@�=q@�1'@�
=@��@�r�@�o@́@�7L@�t�@���@�-@�Q�@�o@�n�@���@š�@�bN@�ff@�@�&�@���@��y@�V@�&�@�Z@���@���@���@��j@�9X@��@�;d@��@�p�@��@�bN@���@�@�O�@��@�r�@��@��@�~�@��@���@���@���@�33@�o@�"�@�+@�"�@�ff@��@�hs@�r�@�Z@�1@�S�@�+@��@��H@��@��!@�{@��@���@��/@�r�@�ƨ@�|�@�"�@��H@��@�-@��-@�7L@�j@��F@�"�@���@�-@�x�@�X@�Ĝ@���@��@���@��@���@�^5@�{@�@�&�@��j@��@��9@�A�@��w@�C�@��H@�{@�$�@���@���@��@���@�bN@�9X@� �@��@��
@�ƨ@��@���@��P@�\)@�33@�ȴ@���@�~�@�E�@�=q@�-@��@���@�?}@�X@�/@�/@���@�(�@��m@��;@�ƨ@�t�@�C�@�33@�o@�\)@��@��@��@���@���@��!@��\@�ff@�E�@�@��@��#@���@��^@��h@��7@�x�@�X@�&�@�%@�Ĝ@��u@�r�@�1'@��@��@�  @�ƨ@��F@��w@�ƨ@��F@��w@��P@�\)@�dZ@�"�@���@��!@�v�@�V@�-@�J@���@��@���@�x�@�hs@�X@�%@���@�1'@��m@��
@��w@���@�|�@�;d@�+@��@��@���@��+@�ff@�M�@��@��T@���@�hs@�O�@�G�@�&�@�V@��j@�r�@�I�@�I�@�1'@�b@��@��@��@��m@��;@��
@�ƨ@��F@���@�|�@�33@�@��H@��R@�J@���@���@���@�`B@�&�@���G�O�@}�N@t��@nC�@f�@`ی@Y`B@RkQ@I�@C� @<tT@7��@2�@+P�@&�8@ C-@��@@�@U�@�@
�@3�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�LB
�5BB	7B	7BPBbB�B�B�B�B#�B8RB~�B��B�9B�oB0!B�B'�BM�BC�B6FBt�B�jB�qB�qB�BBVB/BaHBz�B}�B�B�+B}�Br�BaHB0!B!�BhB�sB��B��B�qB�FB��B�DBz�BbNBM�BW
BaHBr�B�B�\B��B��B�NB'�B0!B.B%�B�B�BDB�B�fB�5B��B�dB��B�+Bt�Bl�BdZBQ�B6FB(�BbB
�B
��B
�?B
��B
��B
�1B
p�B
]/B
B�B
"�B
1B	��B	�B	�B	��B	�-B	��B	��B	�JB	q�B	hsB	e`B	ZB	F�B	:^B	B�B	2-B	�B	�B	1B��B�yB�sB�#B�B��BȴBB��B��B�LB�'B�B��B��B��B��B��B��B��B��B��B�{B�{B�oB�\B�VB�VB�JB�DB�=B�7B�+B�B�B�B�B�B�B�B�B~�B}�B~�B}�B|�B{�By�Bv�Bu�Bs�Bp�Bo�Bm�Bp�Bp�BgmBhsBk�Bn�Bl�BhsBdZBcTBcTBcTBhsBgmBe`BbNBaHB]/B[#BYBXBW
BT�BP�BN�BM�BT�B[#BW
BZB[#BXBVBO�BM�BI�BF�BD�BF�BC�BD�B:^B=qB?}B@�B=qB8RB7LB33B(�B(�B(�B(�B)�B0!BB�B>wB33B+B(�B%�B&�B'�B(�B'�B+B.B1'B33B1'B2-B49B49B7LB9XB>wB?}B?}BD�BD�BB�BA�BC�BE�BF�BH�BK�BL�BM�BO�BQ�BT�BW
BW
BXBZB]/B`BBaHBdZBhsBiyBm�Bp�Br�Bs�Bt�Bt�Bu�By�B�B�B�B�VB�JB�PB�VB�\B�bB��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�'B�'B�3B�FB�RB�dB�}BƨBȴB��B��B��B�B�B�/B�HB�`B�fB�yB�yB�yB�B�B�B�B��B��B��B��B	B	1B	%B	%B	DB	PB	JB	PB	bB	uB	�B	�B	 �B	#�B	$�B	&�B	&�B	(�B	+B	,B	,B	/B	1'B	5?B	7LB	9XB	;dB	<jB	=qB	>wB	?}B	D�B	J�B	M�B	Q�B	VB	XB	XB	YB	ZB	[#B	]/B	_;B	dZB	hsB	hsB	hsB	jB	l�B	n�B	o�B	r�B	u�B	w�B	y�B	{�B	|�B	� B	�B	�B	�B	�%B	�%B	�7B	�=B	�JB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�9B	�?B	�?B	�?B	�LB	�RB	�XB	�jB	�wB	�}B	��B	ÖB	ŢB	ǮB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�)B	�/B	�/B	�/B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�BB	�BB	�HB	�HB	�TB	�TB	�TB	�TB	�ZB	�TB	�TB	�ZB	�`B	�fB	�mB	�fB	�B
 �B
�B
�B
$�B
-CB
5tB
>�B
C{B
I�B
N�B
R:B
YB
b4B
g�B
l�B
m�B
sB
v�B
y>B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
�VB
�PB
�PB
�VB
�PB
�VB
�VB
�VB
�]B
�VB
�VB
�iB
�tB
�]B
�tB
��B
��B
��B
��B
��B�B�BB!B(B.BdB-�Bt�B�yB��B��B%�BBBC`B9$B+�BjFB��B��B��B�B��B�B$�BV�BpZBsmBw�B|�BsmBh*BV�B%�BMB�B��B�]B�B��B��B�wB��BprBW�BCiBL�BV�BhCBx�B��B�UB�B��BuB%�B#�BiB>BB �B�#B��B��B�[B��B��B|�BjRBb"BY�BG�B+�B�BB
�RB
�B
��B
��B
�EB
}�B
fSB
R�B
8BB
�B	��B	�B	�dB	��B	�BB	��B	��B	�nB	�B	gpB	^:B	[(B	O�B	<sB	0*B	8ZB	'�B	�B	QB�B�B�MB�GB��B��B��B��B�hB�\B�bB�&B�B��B��B��B��B��B��B��B��B�qB�qB�ZB�ZB�NB�;B�6B�6B�*B�$B�BB}B{ Bx�By�Bx�Bx�Bv�Bv�Bw�Bt�Bs�Bt�Bs�Br�Bq�Bo�Bl�Bk�Bi�Bf�Be�BcwBf�Bf�B]TB^ZBalBd~BbrB^ZBZBBY<BY<BY<B^[B]UB[HBX6BW1BSBQBOBM�BL�BJ�BF�BD�BC�BJ�BQBL�BPBQBM�BK�BE�BC�B?�B<�B:�B<�B9�B:�B0MB3`B5kB6qB3`B.AB-<B)#B�B�B�B�B�B&B8~B4gB)$B �B�B�B�B�B�B�B �B$B'B)&B'B( B*,B*,B-?B/KB4iB5oB5pB:�B:�B8�B7|B9�B;�B<�B>�BA�BB�BC�BE�BG�BJ�BL�BL�BNBPBS!BV3BW9BZKB^dB_jBc�Bf�Bh�Bi�Bj�Bj�Bk�Bo�Bv�ByB{B�EB�9B�?B�EB�KB�QB�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B� B�3B�>B�PB�iB��B��B��B��B��B��B�B�B�2B�IB�OB�bB�bB�bB�zB�B�B�B�B��B��B��B�B�B�B�B	+B	7B	1B	7B	IB		\B	sB	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	!�B	% B	'B	+#B	-0B	/<B	1HB	2NB	3UB	4[B	5aB	:B	@�B	C�B	G�B	K�B	M�B	M�B	N�B	O�B	QB	SB	UB	Z;B	^TB	^TB	^TB	`_B	bkB	dxB	e~B	h�B	k�B	m�B	o�B	q�B	r�B	u�B	x�B	z�B	z�B	|B	|B	B	�B	�(B	�4B	�MB	�^B	�dB	�jB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�3B	�EB	�QB	�WB	�]B	�pB	�|B	��B	��B	��B	��B	��B	��B	¦B	ìB	ĲB	ŸB	ƾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	� B	� B	�,B	�,B	�,B	�,B	�2B	�,B	�,B	�2B	�8B	�>B	�EG�O�B	��B	��B
�B
�B
dB
#B
+HB
4~B
9NB
?�B
D�B
HB
N�B
XB
]sB
b�B
c�B
h�B
leB
oB
rV11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.17 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.01(+/-0.004) in PSS-78.                                                                                                                                                                                        Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144222022020411442220220204114422  AO  ARCAADJP                                                                    20200619170918    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170918  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170918  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114422  IP                  G�O�G�O�G�O�                