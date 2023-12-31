CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-02-19T18:23:31Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210219182331  20220204114413  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               !A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؏�� 1   @؏����`@8��t��c�S���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    !A   A   A   @�  @���@���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�33B�  B�33B���B���B�  B�33B�  B�  B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)y�D)��D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7y�D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DMy�DM��DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\fD\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�=D�	�D�D��\D��=D��D�UD���D��\D��D�_
D���D��3D�qD�VfDڑHD��HD�RD�\�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��\@�\)@�\)AG�A=G�A]G�A}G�A���A���A���A���AΣ�Aޣ�A��A���BQ�BQ�BQ�BQ�B'Q�B/Q�B7Q�B?Q�BGQ�BOQ�BWQ�B_Q�BgQ�BoQ�Bw�RB�RB���B��)B�B�B�u�B���B��)B���B���B�u�B���B���B���B���B���B��)B���Bè�BǨ�B˨�BϨ�BӨ�Bר�Bۨ�Bߨ�B��B��B��B��B��B���B���B���C�{C�{C�{C�{C	�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C!�{C#�{C%�{C'�{C)�{C+�{C-�{C/�{C1�{C3�{C5�{C7�{C9�{C;�{C=�{C?�{CA��CC�{CE�{CG�{CI�{CK�{CM�{CO�{CQ�{CS�{CU�{CW�{CY�{C[�{C]�{C_�{Ca�{Cc�{Ce�{Cg�{Ci�{Ck�{Cm�{Co�{Cq�{Cs�{Cu�{Cw�{Cy�{C{�{C}�{C�{C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��pC��=C��=C��=C��=C��=C��=C��=C��=C��=C��
C��
C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��pC��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��
C��
C��=C��=C��=C��=C��=C��=D uD �DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D	uD	�D
uD
�DuD�DuD�DuD�DuD�DuD�DuD��DuD�DuD�DuD�DuD�DuD�DuD��DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D uD �D!uD!�D"uD"�D#uD#�D$uD$�D%uD%�D&uD&�D'uD'�D(uD(�D)n�D)�D*uD*�D+uD+�D,uD,�D-uD-�D.uD.�D/uD/�D0uD0�D1uD1�D2uD2�D3uD3�D4uD4�D5uD5�D6uD6�D7n�D7�D8uD8�D9uD9�D:uD:�D;uD;�D<uD<�D=uD=�D>uD>�D?uD?�D@uD@�DAuDA�DBuDB�DCuDC�DDuDD�DEuDE�DFuDF�DGuDG�DHuDH�DIuDI�DJuDJ�DKuDK�DLuDL�DMn�DM�DNuDN�DOuDO�DPuDP�DQuDQ�DRuDR�DSuDS�DTuDT�DUuDU�DVuDV�DWuDW�DXuDX�DYuDY�DZuDZ�D[uD[��D\uD\�D]uD]�D^uD^�D_uD_�D`uD`�DauDa�DbuDb�DcuDc�DduDd�DeuDe�DfuDf�DguDg�DhuDh�DiuDi�DjuDj�DkuDk�DluDl�DmuDm�DnuDn�DouDo�DpuDp�DquDq�DruDr�DsuDs�DtuDt��Dy�\D�)D��D���D���D�{D�O�D��GD���D�pD�Y�D��D���D� D�P�Dڋ�D���D��D�W\D�=D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��mA��`A��`A��HA��;A��A��A��A��A��A��A��A��A��A���A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���Aщ7A�1A�r�A�ȴA�&�A�"�A�"�A�/A���A���A���A�|�A��FA�z�A��A�`BA�ffA�E�A�l�A��A���A�+A��jA�=qA�x�A�ĜA��A��yA�G�A��TA���A��A�ZA�1A���A���A��+A��A�XA�=qA�"�A��A��-A�=qA��^A�1A�O�A�5?A�v�A�A�A�hsA�p�A�A���A�A��A�&�A�O�A���A�9XA��hA�ĜA�S�A���A�^5A�E�A��uA��A��`A�&�A���A�dZA�&�A���A��HA�JA���A���A��-A���A���A��mA�{A���A�1'A���A��!A�I�A�VA��uA�A�;A~(�A}+Az��At�DApI�AnI�Al��Ajn�Ah�DAf�AeG�Ad  A_;dA\bA[�AZ�AX�AWl�AV�DAT��AS?}AR��AQ�AQK�APv�AOp�AM+AJ��AI�;AI�-AI?}AH��AH{AF��AEO�AC�FAA�
AA��AA�A?�A>�jA<v�A:�yA:v�A:$�A9�7A9%A8��A8Q�A7?}A6�A4ȴA2�+A1��A0�HA0��A0�jA0bNA/��A/hsA/33A.�A.n�A-��A,�yA,  A+dZA*��A(^5A'�wA%ƨA$��A$1A#�A!��A!�A r�Ap�A�TA�+Al�A�;At�A�jA�A+AĜA\)A�
A�RA=qA�TA�DA�AhsAO�AG�A
��A	�^An�A��Az�A�A��A�A�Ax�A��A n�@�+@�p�@��H@��@��j@���@�x�@�?}@�%@��@��@�@�^5@�J@�/@�Z@� �@�l�@홚@��@��@�5?@��@�z�@�1'@��@��@�?}@�@�M�@�?}@ߥ�@�ȴ@�$�@�Q�@�n�@ّh@�Ĝ@�9X@�b@ו�@Ցh@�I�@ҸR@�z�@�l�@�n�@�X@�j@���@ˍP@ʏ\@�`B@�r�@Ə\@�X@Ĭ@Ý�@°!@��@��@��@���@���@���@�  @��\@�?}@��`@��D@���@��@�~�@���@� �@�"�@��\@�^5@��@�{@�J@���@���@�hs@��@��9@�1'@�\)@���@��R@��\@���@�Q�@��@���@��@��
@���@�@�M�@���@��D@��m@���@�;d@�~�@�@���@�@��@�?}@�?}@�7L@�/@�V@��@��7@��@��h@�%@�dZ@�p�@��@�V@��@��P@��;@��
@�Z@�@�p�@�/@���@�1'@���@���@��P@�l�@�"�@���@��\@��+@�v�@�{@�%@�bN@�I�@��@��@��F@�;d@��@�b@��@��@���@�r�@� �@�|�@��@��!@�~�@�{@��u@�bN@���@�1'@�ff@�@�p�@�%@��@��7@��-@��-@�?}@�Ĝ@��7@��#@���@��#@��#@��-@��h@�7L@�/@�7L@�7L@�%@���@�A�@���@�^5@�J@��@���@���@���@��y@��@�7L@��@�7L@���@�%@���@��@��@���@�M�@��@���@��-@��7@�`B@�?}@�V@���@���@��j@��@�z�@�  @���@�ƨ@��w@��@��@���@�1@� �@��@��m@���@��w@��F@���@���@��@��@��+@�ff@��R@�5?@���@��T@���@��-@�p�@��h@�X@�7L@�7L@���@�r�@�Xy@��u@y�C@m�)@d��@[{J@V�@M�h@G�@B��@=�j@7�@0I�@,�.@'��@#�@�h@�a@�@�@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��mA��`A��`A��HA��;A��A��A��A��A��A��A��A��A��A���A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���Aщ7A�1A�r�A�ȴA�&�A�"�A�"�A�/A���A���A���A�|�A��FA�z�A��A�`BA�ffA�E�A�l�A��A���A�+A��jA�=qA�x�A�ĜA��A��yA�G�A��TA���A��A�ZA�1A���A���A��+A��A�XA�=qA�"�A��A��-A�=qA��^A�1A�O�A�5?A�v�A�A�A�hsA�p�A�A���A�A��A�&�A�O�A���A�9XA��hA�ĜA�S�A���A�^5A�E�A��uA��A��`A�&�A���A�dZA�&�A���A��HA�JA���A���A��-A���A���A��mA�{A���A�1'A���A��!A�I�A�VA��uA�A�;A~(�A}+Az��At�DApI�AnI�Al��Ajn�Ah�DAf�AeG�Ad  A_;dA\bA[�AZ�AX�AWl�AV�DAT��AS?}AR��AQ�AQK�APv�AOp�AM+AJ��AI�;AI�-AI?}AH��AH{AF��AEO�AC�FAA�
AA��AA�A?�A>�jA<v�A:�yA:v�A:$�A9�7A9%A8��A8Q�A7?}A6�A4ȴA2�+A1��A0�HA0��A0�jA0bNA/��A/hsA/33A.�A.n�A-��A,�yA,  A+dZA*��A(^5A'�wA%ƨA$��A$1A#�A!��A!�A r�Ap�A�TA�+Al�A�;At�A�jA�A+AĜA\)A�
A�RA=qA�TA�DA�AhsAO�AG�A
��A	�^An�A��Az�A�A��A�A�Ax�A��A n�@�+@�p�@��H@��@��j@���@�x�@�?}@�%@��@��@�@�^5@�J@�/@�Z@� �@�l�@홚@��@��@�5?@��@�z�@�1'@��@��@�?}@�@�M�@�?}@ߥ�@�ȴ@�$�@�Q�@�n�@ّh@�Ĝ@�9X@�b@ו�@Ցh@�I�@ҸR@�z�@�l�@�n�@�X@�j@���@ˍP@ʏ\@�`B@�r�@Ə\@�X@Ĭ@Ý�@°!@��@��@��@���@���@���@�  @��\@�?}@��`@��D@���@��@�~�@���@� �@�"�@��\@�^5@��@�{@�J@���@���@�hs@��@��9@�1'@�\)@���@��R@��\@���@�Q�@��@���@��@��
@���@�@�M�@���@��D@��m@���@�;d@�~�@�@���@�@��@�?}@�?}@�7L@�/@�V@��@��7@��@��h@�%@�dZ@�p�@��@�V@��@��P@��;@��
@�Z@�@�p�@�/@���@�1'@���@���@��P@�l�@�"�@���@��\@��+@�v�@�{@�%@�bN@�I�@��@��@��F@�;d@��@�b@��@��@���@�r�@� �@�|�@��@��!@�~�@�{@��u@�bN@���@�1'@�ff@�@�p�@�%@��@��7@��-@��-@�?}@�Ĝ@��7@��#@���@��#@��#@��-@��h@�7L@�/@�7L@�7L@�%@���@�A�@���@�^5@�J@��@���@���@���@��y@��@�7L@��@�7L@���@�%@���@��@��@���@�M�@��@���@��-@��7@�`B@�?}@�V@���@���@��j@��@�z�@�  @���@�ƨ@��w@��@��@���@�1@� �@��@��m@���@��w@��F@���@���@��@��@��+@�ff@��R@�5?@���@��T@���@��-@�p�@��h@�X@�7L@�7L@���@�r�@�Xy@��u@y�C@m�)@d��@[{J@V�@M�h@G�@B��@=�j@7�@0I�@,�.@'��@#�@�h@�a@�@�@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB%BDB,B@�BI�BL�BO�BP�BS�BT�BO�BP�BT�BbNB^5B_;BbNBe`BiyBp�Bt�By�By�By�B|�By�By�Bw�Bv�Bw�Bu�Bu�Bu�Bt�Bq�Bt�Bv�Bx�B}�Bx�Bx�Bx�Bz�By�Bv�Bs�BbNBQ�BO�BC�B1'B#�B{BJB%B��B�B�fB�)B��B��B��BȴBÖB��B�LB��B�PBm�BXBF�B?}B:^B7LB33B'�B�BPB
��B
��B
�B
�BB
��B
ǮB
��B
�^B
�B
��B
�bB
�JB
�B
x�B
k�B
\)B
R�B
@�B
�B	�B	�fB	�
B	ǮB	�^B	��B	��B	��B	|�B	hsB	cTB	`BB	^5B	ZB	VB	R�B	J�B	F�B	@�B	:^B	49B	-B	(�B	 �B	�B	�B	�B	�B	oB	JB	B��B�B�B�B�NB�5B�
B��BɺBǮBĜB��B�wB�jB�XB�-B�B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�VB�JB�JB�%B�B}�Bw�Bu�Br�Bp�Bn�Bk�BhsBcTB[#BZBXBT�BR�BO�BN�BK�BF�BB�B?}B?}B=qB:^B7LB7LB6FB6FB5?B49B2-B1'B1'B0!B/B/B.B.B-B,B)�B(�B)�B&�B'�B&�B&�B%�B%�B&�B%�B$�B$�B$�B&�B'�B'�B'�B+B/B1'B2-B2-B5?B6FB6FB6FB9XB<jB;dB=qB?}B?}B>wB?}B?}B>wB?}B?}B?}B@�BB�BC�BF�BJ�BM�BN�BP�BQ�BQ�BR�BT�BVBYB\)B`BBaHBdZBffBgmBgmBgmBgmBhsBiyBk�Bo�Br�Br�Bs�Bv�Bw�By�B|�B�B�B�+B�1B�7B�=B�=B�=B�DB�JB�PB�VB�bB�uB�{B��B��B��B��B��B��B��B��B��B�B�3B�?B�jB��BBĜBƨBȴB��B��B�
B�B�B�B�)B�5B�ZB�B�B��B��B��B�B��B��B��B��B��B	B	+B	�B	�B	 �B	!�B	"�B	$�B	.B	1'B	2-B	5?B	9XB	<jB	=qB	=qB	>wB	?}B	@�B	A�B	A�B	A�B	@�B	>wB	:^B	5?B	0!B	2-B	2-B	5?B	7LB	9XB	=qB	?}B	@�B	A�B	D�B	E�B	I�B	R�B	M�B	M�B	O�B	M�B	R�B	\)B	`BB	bNB	ffB	k�B	q�B	x�B	z�B	|�B	� B	�B	�B	�B	�B	�B	�B	�%B	�1B	�+B	�B	�B	�B	�B	�=B	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�-B	�9B	�?B	�FB	�RB	�XB	�^B	�qB	��B	ĜB	ŢB	ǮB	ɺB	��B	��B	��B	��B	ɺB	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�BB	�B
 4B
xB
�B
#B
$�B
,"B
5tB
:^B
@�B
E�B
K�B
O�B
U2B
Z�B
abB
c�B
i�B
o�B
t111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�uB�uB�uB�uB�|B�uB�uB�|B�|B�|B�|B�|B�|B�|B�|B�|B�|B�|B�|B�|B�|B�|B�|B��B��B�|B��B��B�|B��B��B��B��B�B#iB7�BABD.BG@BHFBKYBL_BGABHGBL`BY�BU�BV�BY�B\�B`�BhBlBq=Bq=Bq=BtPBq=Bq>Bo2Bn,Bo2Bm&Bm&Bm&Bl BiBl Bn-Bp9BuXBp9Bp9Bp9BrEBq?Bn-BkBY�BISBGFB:�B(�BBB�B�B��B�hB�B��BәB�oB�oB�]B�&B�	B��B��B�XB��Be
BO�B>$B6�B1�B.�B*�BoB1B�B
�|B
�WB
�!B
��B
�wB
�4B
�B
��B
��B
�B
��B
��B
|�B
paB
cB
S�B
J�B
8B
FB	�MB	��B	ΣB	�HB	��B	��B	�\B	�+B	t�B	`B	Z�B	W�B	U�B	Q�B	M�B	J�B	BfB	>MB	8)B	2B	+�B	$�B	 �B	nB	UB	IB	CB	+B	
B	�B��B�B�KB�8B�,B��B��BιBŃB�jB�_B�MB�:B�(B�B�
B��B��B��B�B�rB�fB�fB�fB�[B�OB�HB�BB�<B�0B�B�B� B� B}�Bz�Bu�Bo�Bm|BjiBh]BfQBc?B`-B[BR�BQ�BO�BL�BJ�BG�BF�BC�B>fB:NB7<B7<B50B2B/B/B.B.B,�B+�B)�B(�B(�B'�B&�B&�B%�B%�B$�B#�B!�B �B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B&�B(�B)�B)�B-B.	B.	B.	B1B4-B3'B54B7@B7@B6:B7AB7AB6;B7AB7AB7AB8GB:SB;ZB>lBB�BE�BF�BH�BI�BI�BJ�BL�BM�BP�BS�BXBYB\B^)B_0B_0B_0B_0B`6Ba<BcHBgaBjsBjsBkyBn�Bo�Bq�Bt�By�B|�B~�B�B��B��B��B��B�B�B�B�B�#B�6B�<B�BB�BB�[B��B��B��B��B��B��B��B��B��B�)B�HB�NB�[B�gB�sBÅBȣB��B��B��B��B��B��B�B�AB�fB�B�B�B�fB�xB��B�B�B��B��B��B	AB	qB	~B	�B	�B	�B	%�B	(�B	)�B	,�B	1B	4"B	5)B	5)B	6/B	75B	8;B	9AB	9AB	9AB	8;B	6/B	2B	,�B	'�B	)�B	)�B	,�B	/B	1B	5*B	76B	8;B	9AB	<TB	=ZB	ArB	J�B	E�B	E�B	G�B	E�B	J�B	S�B	W�B	ZB	^B	c;B	i`B	p�B	r�B	t�B	w�B	{�B	{�B	{�B	{�B	{�B	|�B	}�B	�B	~�B	|�B	x�B	y�B	|�B	��B	�B	�FB	�kB	�kB	�YB	�YB	�YB	�eB	�xB	�eB	�eB	�eB	�eB	�kB	�qB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�:B	�MB	�SB	�_B	�kB	�rB	�rB	�rB	�rB	�kB	�rB	�kB	�rB	�~B	�xB	�xB	�~B	�~B	ńB	ǐB	ʢB	̮B	˨B	˨B	̮B	ʹB	��B	�1B	��B
&B
bB
�B
<B
#�B
- B
2
B
8�B
=4B
C�B
G�B
L�B
RB
YB
[�B
a�B
gbB
k�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.17 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144132022020411441320220204114413  AO  ARCAADJP                                                                    20210219182331    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210219182331  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210219182331  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114413  IP                  G�O�G�O�G�O�                