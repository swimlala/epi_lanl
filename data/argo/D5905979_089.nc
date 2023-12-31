CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:15Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170915  20220204114419  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               YA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @��\r��V1   @��]  F@7��+�c�7KƧ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    YA   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D   D � D!fD!� D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7�fD8  D8� D9  D9� D:  D:� D;fD;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DNfDN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� DgfDg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy��D��D�[�D��\D�� D��D�c�D��
D�ФD�\D�P�D���D��=D�'\D�\)Dښ=D���D�D�P�D��D��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�(�@��\@��\AG�A=G�A]G�A}G�A���A���A���A�p�AΣ�Aޣ�A��A���BQ�BQ�BQ�BQ�B'Q�B/Q�B7Q�B?Q�BGQ�BOQ�BWQ�B_Q�BgQ�BoQ�BwQ�BQ�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bè�BǨ�B˨�BϨ�BӨ�Bר�Bۨ�Bߨ�B��B��B��B��B��B���B���B���C�{C�{C�{C�{C	�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C!�{C#�{C%�{C'�{C)�{C+�{C-�{C/�{C1�{C3�{C5�{C7�{C9�{C;�{C=�{C?�{CA�{CC�{CE�{CG�{CI�{CK�{CM�{CO�{CQ�{CS�{CU�CW�{CY�{C[�{C]�{C_�{Ca�Cc�{Ce�{Cg�{Ci�{Ck�{Cm�{Co�{Cq�{Cs�{Cu�{Cw�{Cy�{C{�{C}�{C�{C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��
C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��
C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��pC��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��pD uD �DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D	uD	�D
uD
�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D uD ��D!uD!�D"uD"�D#uD#�D${�D$�D%uD%�D&uD&�D'uD'�D(uD(�D)uD)�D*uD*�D+uD+�D,uD,�D-uD-�D.uD.�D/uD/�D0uD0�D1uD1�D2uD2�D3uD3�D4uD4�D5uD5�D6uD6�D7{�D7�D8uD8�D9uD9�D:uD:��D;uD;�D<uD<�D=uD=�D>uD>�D?uD?�D@uD@�DAuDA�DBuDB�DCuDC�DDuDD�DEuDE�DFuDF�DGuDG�DHuDH�DIuDI�DJuDJ�DKuDK�DLuDL�DMuDM��DNuDN�DOuDO�DPuDP�DQuDQ�DRuDR�DSuDS�DTuDT�DUuDU�DVuDV�DWuDW�DXuDX�DYuDY�DZuDZ�D[uD[�D\uD\�D]uD]�D^uD^�D_uD_�D`uD`�DauDa�DbuDb�DcuDc�DduDd�DeuDe�DfuDf��DguDg�DhuDh�DiuDi�DjuDj�DkuDk�DluDl�DmuDm�DnuDn�DouDo�Dpn�Dp�DquDq�DruDr�DsuDs�DtuDtιDy��D�\D�VfD���D�ڏD�\D�^D���D��3D��D�K�D��\D���D�!�D�V�Dڔ�D��D��D�K�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A�"�A�"�A�"�A�+A�/A�(�A�$�A��A�oA�1A�ƨA�O�A�1'A���A֙�A�K�A��HAԙ�A�(�A�A�A���A��mA��A���A�S�A��Aˣ�A�C�A���Aɛ�A�oA�1A��mA�1'Aŏ\AđhA��A���A��hA�5?A���A��-A�G�A���A���A�x�A�&�A���A�33A�bNA�oA��/A���A�ZA���A�dZA�
=A��;A���A���A�  A���A���A�ƨA��DA���A�z�A���A���A�M�A��A���A�ZA���A�ZA���A��PA�l�A�I�A�A���A���A�ffA�?}A�VA��mA�A�v�A�C�A��A���A���A�\)A���A�33A���A�&�A�ZA���A��uA��7A��A��`A���A��A��hA�VA��A��!A��FA�=qA�dZA��HA���A��A���A+Az�`Ayt�AvbNAr��Aqx�Apz�AoAlbAe�AdE�Ac`BAbr�A`��A_S�A]&�AZ�yAY�AX��AW��AV�jAVA�AV(�AVAUdZAT�HAT�DAQ�-AN�!AM��AM�AL��AK�AKl�AJ�AI��AEl�ACƨAC�hAC"�AAp�A@Q�A=�A;�7A:�uA9ƨA9&�A7��A7&�A6�A6�A6n�A5x�A4��A3��A2$�A/��A-�
A,v�A*��A*�A* �A)�hA'\)A%ƨA$�yA#S�A"  A �RA AS�Av�A�A�FA+A��A��A��A\)A%A��A��A9XA|�AVA�uAI�A��A�Ar�A�wA�RA$�At�A�A��AĜA��AA�AS�A	�wA�A�yA�A
=A^5AA��A`BA z�@���@�S�@�~�@��^@��@��;@��@��@�v�@�p�@���@�&�@�Ĝ@��m@�+@��@�P@�?}@�Q�@���@�E�@�hs@���@�5?@�p�@�@ݡ�@�7L@�r�@۶F@�S�@��@��@��@ڧ�@�O�@ՙ�@�S�@�ff@Ѻ^@�j@�~�@̋D@���@�C�@ʟ�@��#@ɡ�@�/@��`@ț�@�Q�@�ƨ@�t�@�K�@�;d@ǅ@�9X@���@�A�@��@Ǖ�@�\)@���@���@�{@�Z@��H@�M�@�@�`B@���@��@�bN@�I�@��m@�dZ@�ff@���@�l�@�n�@��^@��@�@�|�@�p�@� �@��@��;@�ƨ@���@��@�  @���@��P@�o@���@�M�@�X@�z�@�S�@��@���@��#@��@�Q�@���@��@��+@�^5@�5?@�p�@��/@��@��u@�b@�  @��;@�b@�r�@��D@�bN@��@�\)@��!@�7L@�O�@��@���@�@�-@�{@���@��^@���@�hs@�p�@�X@��7@��^@���@��@���@�Ĝ@��@�z�@�9X@�K�@��@���@�V@�{@��@���@���@�x�@���@��7@�hs@���@�(�@��@���@���@���@�|�@�K�@�
=@�ȴ@���@��+@�~�@�v�@�^5@�E�@�$�@��#@��@���@�bN@�Z@��@��@��F@�S�@��@��@���@���@��!@�v�@�5?@��@��h@�hs@�O�@�G�@���@�1'@�ƨ@��P@��P@�|�@�"�@��!@�M�@�E�@�n�@�E�@��T@��7@�/@��/@�z�@�b@�1@���@��@��@��;@�ƨ@��F@�l�@��@�ff@�=q@��#@���@�x�@�V@���@�j@�  @��w@��@�33@��@��H@���@��!@�v�@��@��#@���@�hs@��@��j@�j@�1@���@��;@��w@���@�dZ@�"�@��y@��\@�E�@�@��^@���@��j@~ȴ@x��@m�@g�@_{J@Y<6@NH�@FZ�@?��@;��@4��@/C�@+(@&p;@!��@�j@�H@ȴ@��@.I111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��A��A�"�A�"�A�"�A�+A�/A�(�A�$�A��A�oA�1A�ƨA�O�A�1'A���A֙�A�K�A��HAԙ�A�(�A�A�A���A��mA��A���A�S�A��Aˣ�A�C�A���Aɛ�A�oA�1A��mA�1'Aŏ\AđhA��A���A��hA�5?A���A��-A�G�A���A���A�x�A�&�A���A�33A�bNA�oA��/A���A�ZA���A�dZA�
=A��;A���A���A�  A���A���A�ƨA��DA���A�z�A���A���A�M�A��A���A�ZA���A�ZA���A��PA�l�A�I�A�A���A���A�ffA�?}A�VA��mA�A�v�A�C�A��A���A���A�\)A���A�33A���A�&�A�ZA���A��uA��7A��A��`A���A��A��hA�VA��A��!A��FA�=qA�dZA��HA���A��A���A+Az�`Ayt�AvbNAr��Aqx�Apz�AoAlbAe�AdE�Ac`BAbr�A`��A_S�A]&�AZ�yAY�AX��AW��AV�jAVA�AV(�AVAUdZAT�HAT�DAQ�-AN�!AM��AM�AL��AK�AKl�AJ�AI��AEl�ACƨAC�hAC"�AAp�A@Q�A=�A;�7A:�uA9ƨA9&�A7��A7&�A6�A6�A6n�A5x�A4��A3��A2$�A/��A-�
A,v�A*��A*�A* �A)�hA'\)A%ƨA$�yA#S�A"  A �RA AS�Av�A�A�FA+A��A��A��A\)A%A��A��A9XA|�AVA�uAI�A��A�Ar�A�wA�RA$�At�A�A��AĜA��AA�AS�A	�wA�A�yA�A
=A^5AA��A`BA z�@���@�S�@�~�@��^@��@��;@��@��@�v�@�p�@���@�&�@�Ĝ@��m@�+@��@�P@�?}@�Q�@���@�E�@�hs@���@�5?@�p�@�@ݡ�@�7L@�r�@۶F@�S�@��@��@��@ڧ�@�O�@ՙ�@�S�@�ff@Ѻ^@�j@�~�@̋D@���@�C�@ʟ�@��#@ɡ�@�/@��`@ț�@�Q�@�ƨ@�t�@�K�@�;d@ǅ@�9X@���@�A�@��@Ǖ�@�\)@���@���@�{@�Z@��H@�M�@�@�`B@���@��@�bN@�I�@��m@�dZ@�ff@���@�l�@�n�@��^@��@�@�|�@�p�@� �@��@��;@�ƨ@���@��@�  @���@��P@�o@���@�M�@�X@�z�@�S�@��@���@��#@��@�Q�@���@��@��+@�^5@�5?@�p�@��/@��@��u@�b@�  @��;@�b@�r�@��D@�bN@��@�\)@��!@�7L@�O�@��@���@�@�-@�{@���@��^@���@�hs@�p�@�X@��7@��^@���@��@���@�Ĝ@��@�z�@�9X@�K�@��@���@�V@�{@��@���@���@�x�@���@��7@�hs@���@�(�@��@���@���@���@�|�@�K�@�
=@�ȴ@���@��+@�~�@�v�@�^5@�E�@�$�@��#@��@���@�bN@�Z@��@��@��F@�S�@��@��@���@���@��!@�v�@�5?@��@��h@�hs@�O�@�G�@���@�1'@�ƨ@��P@��P@�|�@�"�@��!@�M�@�E�@�n�@�E�@��T@��7@�/@��/@�z�@�b@�1@���@��@��@��;@�ƨ@��F@�l�@��@�ff@�=q@��#@���@�x�@�V@���@�j@�  @��w@��@�33@��@��H@���@��!@�v�@��@��#@���@�hs@��@��j@�j@�1@���@��;@��w@���@�dZ@�"�@��y@��\@�E�@�@��^G�O�@��j@~ȴ@x��@m�@g�@_{J@Y<6@NH�@FZ�@?��@;��@4��@/C�@+(@&p;@!��@�j@�H@ȴ@��@.I111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	ȴB	ÖB	�dB	�RB	�XB	�jB	�dB	�XB	�}B	��B	��B	�ZB
bB
uB
#�B
7LB
E�B
ffB
n�B
}�B
� B
��B
�?B
��B
�B
�#B
�BB
�sB
�B,BH�BT�B[#BaHBk�B~�B�ZB+B<jB@�BL�BffB�1B��B��B��B�B��B�DB�B{�By�Br�Bp�Bs�B{�Bz�B}�B� B�B�VB�+B�=B�=B�=B�7B�1B�%B�B�B� B|�By�Bv�BjBcTBP�BD�B=qB0!B(�B�BbB�)B��B�uB�+Bx�BiyBN�B7LB
��B
��B
�B
r�B
\)B
C�B
�B	��B	�B	�mB	�fB	�`B	�;B	�B	ƨB	��B	�\B	�DB	�B	{�B	w�B	q�B	`BB	\)B	S�B	M�B	I�B	E�B	D�B	B�B	@�B	=qB	;dB	6FB	$�B	 �B	�B	�B	�B	oB	VB	+B�B�sB�fB�fB�NB�BB�)B��B��BɺBǮB��B�dB�^B�RB�FB�3B�B�B��B��B��B��B�bB�VB�PB�=B�+B}�B{�Bx�Bs�Bp�Bm�BjBiyBgmBcTB`BB^5B^5BZBW
BVBT�BT�BS�BR�BQ�BP�BO�BO�BL�BI�BH�BG�BE�BE�BD�BC�BC�BB�BB�BB�BC�B<jB<jB8RB8RB7LB6FB6FB7LB6FB6FB5?B7LB6FB6FB6FB7LB8RB6FB9XB;dB;dB:^B:^B8RB8RB:^B7LB6FB5?B5?B5?B49B6FB49B9XB9XB:^B<jB<jB=qB=qB=qB<jB<jB>wBA�BC�BC�BB�BC�BE�BK�BL�BN�BP�BT�BT�BXBZB[#B`BBe`BffBffBgmBm�Bw�B�B�=B�7B�=B�JB�\B�hB��B��B��B�{B�{B�uB�{B�{B�uB�uB�uB�oB�hB�hB��B�uB�bB�JB�PB�DB�VB�bB�bB�uB��B��B��B��B��B�B�?B�LB�RB�jB��BƨBƨBǮB��B��B��B�B�5B�NB�ZB�`B�B�B�B�B�B��B		7B	DB	PB	hB	uB	�B	�B	�B	�B	"�B	%�B	(�B	+B	-B	.B	.B	.B	5?B	8RB	=qB	@�B	A�B	E�B	H�B	J�B	O�B	S�B	W
B	ZB	ZB	^5B	`BB	aHB	dZB	e`B	ffB	ffB	gmB	iyB	k�B	o�B	p�B	r�B	s�B	u�B	v�B	w�B	w�B	y�B	|�B	� B	�B	�B	�B	�B	�%B	�+B	�1B	�7B	�=B	�JB	�VB	�\B	�hB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�-B	�FB	�LB	�LB	�LB	�LB	�LB	�RB	�RB	�dB	�}B	�}B	�}B	�}B	�}B	�wB	�}B	��B	��B	B	ÖB	ĜB	ŢB	ƨB	ǮB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�)B	��B	�B
}B
�B
#B
"�B
0�B
/iB
;B
C{B
L�B
O�B
U�B
XEB
\]B
_VB
c�B
jB
o�B
s�B
tT111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�bB	�PB	�WB	�iB	�cB	�WB	�|B	¿B	��B	�WB
\B
oB
�B
/DB
=�B
^\B
f�B
u�B
w�B
��B
�1B
��B
� B
�B
�1B
�bB
�B#�B@�BL�BSBY0BcmBv�B�:B"�B4EB8^BD�B^?B�B�|B�]B��B��B�dB�Bx�Bs�Bq�Bj�Bh~Bk�Bs�Br�Bu�Bw�B{�B�/BB�B�B�B�B�B}�B|�By�Bw�Bt�Bq�Bn�Bb[B[1BH�B<{B5QB(B �B�BEB�B��B�aBBp�BahBF�B/?B
��B
��B
��B
j�B
T+B
;�B
�B	��B	�B	�xB	�rB	�lB	�GB	�B	��B	��B	�oB	�WB	}2B	s�B	o�B	i�B	XXB	T@B	LB	E�B	A�B	=�B	<�B	:�B	8�B	5�B	3}B	.`B	�B	�B	�B	�B	�B	
�B	sB�IB��B��BކBއB�oB�cB�KB�B��B��B��B��B��B��B�wB�kB�XB�AB�(B�	B��B��B��B��B�B�yB�fBUBvBtBq Bk�Bh�Be�Bb�Ba�B_�B[�BXpBVcBVcBRKBO9BN3BM-BM-BL'BK!BJBIBHBHBD�BA�B@�B?�B=�B=�B<�B;�B;�B:�B:�B:�B;�B4�B4�B0�B0�B/B.yB.yB/B.zB.zB-sB/�B.zB.zB.zB/�B0�B.zB1�B3�B3�B2�B2�B0�B0�B2�B/�B.{B-tB-uB-uB,oB.|B,oB1�B1�B2�B4�B4�B5�B5�B5�B4�B4�B6�B9�B;�B;�B:�B;�B=�BC�BEBGBIBM4BM4BPEBRRBSXBXwB]�B^�B^�B_�Be�BpB}RB�pB�jB�pB�|B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�}B��B�xB��B��B��B��B��B��B��B��B�B�@B�qB�~B��B��B��B��B��B��B��B�
B�(B�@B�eB�~B܊BݐB�B�B�B��B��B�B	eB	qB	}B		�B	�B	�B	�B	�B	�B	�B	B	!"B	#.B	%9B	&?B	&?B	&?B	-jB	0}B	5�B	8�B	9�B	=�B	@�B	B�B	HB	L!B	O3B	RFB	RFB	V^B	XkB	YqB	\�B	]�B	^�B	^�B	_�B	a�B	c�B	g�B	h�B	j�B	k�B	m�B	n�B	o�B	o�B	rB	uB	x'B	z3B	{9B	|@B	}FB	~LB	RB	�XB	�]B	�cB	�pB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�3B	�3B	�9B	�9B	�@B	�RB	�jB	�pB	�pB	�pB	�pB	�pB	�vB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�:B	�@B	�FG�O�B	�!B	�#B
�B
�B
CB
�B
(�B
'�B
3�B
;�B
D�B
G�B
NB
PcB
T{B
WtB
[�B
b�B
g�B
k�B
lr111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.17 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.005) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144192022020411441920220204114419  AO  ARCAADJP                                                                    20200619170915    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170915  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170915  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114419  IP                  G�O�G�O�G�O�                