CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:13Z creation      
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
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20200619170913  20220204114419  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               SA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @���2@�_1   @����l& @6�-V�c����+1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    SA   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffBffB��B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� DfD� D  D� D  D� D  D�fD  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@�fDA  DA� DB  DB� DC  DC� DD  DD�fDE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DL��DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� De��Dfy�Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dmy�Dm��Dny�Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�Dt  DtffDy��D�3D�^fD�� D���D��D�ND��)D��D�$)D�U�D�� D��)D��D�YHDچfD��D�!HD�X D�qD��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�(�@��\@��\AG�A=G�A]G�A}G�A���A���A���A���AΣ�Aޣ�A��A���B�RB�RB�B�B'Q�B/Q�B7Q�B?Q�BGQ�BOQ�BWQ�B_Q�BgQ�BoQ�BwQ�BQ�B���B���B���B���B��)B��)B���B���B���B���B���B���B���B���B���B���Bè�BǨ�B˨�BϨ�BӨ�Bר�Bۨ�Bߨ�B��B��B��B��B��B���B��)B��*C�{C�{C�{C�{C	�{C�{C�{C�{C�{C�{C�{C�{C�{C�C�{C�{C!�{C#�{C%�{C'�{C)�{C+�{C-�{C/�{C1�{C3�{C5�C7�{C9�{C;�{C=�{C?�{CA�{CC�{CE�{CG�{CI�{CK�{CM�{CO�{CQ�{CS�{CU�{CW�{CY�{C[�{C]�{C_�{Ca�{Cc�{Ce�{Cg�{Ci�{Ck�{Cm�{Co�{Cq�{Cs�{Cu�{Cw�{Cy�{C{�{C}�{C�{C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��
C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��pC��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��pC��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=D uD �DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D	uD	�D
uD
��DuD�DuD�DuD�D{�D�DuD��D{�D�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D uD �D!uD!�D"uD"�D#uD#�D$uD$�D%uD%�D&uD&�D'uD'�D(uD(�D)uD)�D*uD*�D+uD+�D,uD,�D-uD-�D.uD.�D/uD/�D0uD0�D1uD1�D2uD2�D3uD3�D4uD4�D5uD5�D6uD6�D7uD7�D8uD8�D9uD9�D:uD:�D;uD;�D<uD<�D=uD=�D>uD>�D?uD?�D@{�D@�DAuDA�DBuDB�DCuDC�DD{�DD�DEuDE�DFuDF�DGuDG�DHuDH�DIuDI�DJuDJ�DKuDK�DLuDL�DMuDM�DNuDN�DOuDO�DPuDP�DQuDQ�DRuDR�DSuDS�DTuDT�DUuDU�DVuDV�DWuDW�DXuDX�DYuDY�DZuDZ�D[uD[�D\uD\�D]uD]�D^uD^�D_uD_�D`uD`�DauDa�DbuDb�DcuDc�DduDd�DeuDe�Dfn�Df�DguDg�DhuDh�DiuDi�DjuDj�DkuDk�DluDl�Dmn�Dm�Dnn�Dn�DouDo�DpuDp�DquDq�DruDr�Dsn�Ds�Dt[�Dy��D��D�X�D���D��\D�\D�H�D�~�D��3D��D�PRD���D�ָD�)D�S�Dڀ�D��)D��D�R�D� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�~�A҉7A҉7A҉7A҉7A҃A҅A҅A҉7A�v�A���A�S�A�ZA��A�%A��
AύPA���A��HA�A��A�K�A�t�A�ZA�|�A�(�A�hsA�"�A�=qA�5?A��A��HA�hsA��A�oA��mA�r�A�~�A��7A�jA��;A�jA�/A�{A��mA��A�bA��FA�v�A���A�oA��!A�{A���A�XA�&�A�ȴA���A�(�A�z�A�"�A���A��RA�&�A�  A��mA�/A��wA�oA��A�|�A��HA�|�A���A��/A��A�$�A��DA�7LA���A��A�"�A��mA��wA�;dA��\A�bNA�"�A��TA�x�A��A��\A��HA��;A�7LA���A�%A���A�$�A�A�M�A���A��A���A�JA��!A�\)A��A�E�A�G�A���A�A���A���A�A�AoAt�A�A~�A{�TAz9XAx�Aw�Av{At5?Ap��Ao`BAnjAln�AlbAj��Ah�RAgx�AfĜAf(�AeAe|�Ae?}Ad��Ac�Aat�A_��A]��A\��AY�hAW�AS��AQVAP(�AP9XAO��AO�;AOK�AN^5AM\)ALr�AJ  AGO�AE�AD1'ACXAAO�A?dZA="�A;��A:��A9�wA7��A5��A4ĜA3�7A3�A2��A1�FA0~�A.�/A-�#A+��A*�A*z�A)XA(A'�7A%��A$��A$^5A"�/A!�;A �`A`BA��AO�A�/AI�AE�AQ�A��A��A�A��AI�AȴA|�AbNA��AȴA1A�FAȴAQ�AS�A1'A7LA
1'A	�-A�RA~�AVA�FA�yAr�Ax�A�AVA�AS�A �D@��+@�ff@���@�
=@���@��@�@�|�@�S�@�K�@�S�@��y@���@�(�@��@��@睲@�5?@�@�u@�+@�^5@�I�@�o@�@ݺ^@�p�@܃@۝�@ڇ+@�hs@�Ĝ@׍P@֟�@�5?@ղ-@��/@�o@�V@�$�@�?}@ύP@�dZ@�"�@�-@�bN@�|�@�
=@��@�O�@�z�@�  @Ǯ@�dZ@��@�1@¸R@���@���@��@�v�@�G�@��/@�A�@�C�@�5?@�hs@�z�@�"�@�^5@���@���@��;@�\)@�+@�V@���@�@�hs@���@��D@�1'@��P@�l�@���@��!@�$�@��@�hs@�&�@�Z@���@�|�@��@��7@�@�33@�@�r�@���@�V@���@�  @��
@�K�@��y@��!@��y@���@�~�@�=q@���@���@���@��h@�X@���@��!@�C�@�ƨ@�Z@�j@�j@��9@��h@�M�@�^5@�$�@��@�/@��`@� �@��;@���@�33@��+@�{@���@�7L@��@���@�O�@�V@��/@�  @�$�@�%@�Z@� �@�9X@��@��!@��#@��#@�E�@�=q@�$�@�J@���@�1'@��m@��@���@��P@�S�@�"�@��@��\@�^5@�^5@�5?@�-@�J@�@���@���@��h@�X@�?}@�&�@��@�I�@��
@�l�@�"�@���@��@��!@��\@�n�@�M�@�J@���@��-@��@��@��/@�b@��m@��
@�ƨ@�1'@��@�\)@��@�=q@���@��h@�X@�?}@��@��`@��j@��@�1@���@�|�@�dZ@�S�@�
=@��H@���@�^5@�$�@��@��-@���@��7@�p�@�X@�O�@�7L@���@���@�Ĝ@��@��D@�Z@� �@��@��
@��F@��@��@�t�@�;d@���@��@���@��R@���@���@�~�@�ff@�V@��@��@�@��7@�&�@���@���@C@v�'@k�P@d�D@[�	@SC�@L��@D7@=��@6E�@.��@)c@$]d@�"@�2@�b@��@�m@v`@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�~�A҉7A҉7A҉7A҉7A҃A҅A҅A҉7A�v�A���A�S�A�ZA��A�%A��
AύPA���A��HA�A��A�K�A�t�A�ZA�|�A�(�A�hsA�"�A�=qA�5?A��A��HA�hsA��A�oA��mA�r�A�~�A��7A�jA��;A�jA�/A�{A��mA��A�bA��FA�v�A���A�oA��!A�{A���A�XA�&�A�ȴA���A�(�A�z�A�"�A���A��RA�&�A�  A��mA�/A��wA�oA��A�|�A��HA�|�A���A��/A��A�$�A��DA�7LA���A��A�"�A��mA��wA�;dA��\A�bNA�"�A��TA�x�A��A��\A��HA��;A�7LA���A�%A���A�$�A�A�M�A���A��A���A�JA��!A�\)A��A�E�A�G�A���A�A���A���A�A�AoAt�A�A~�A{�TAz9XAx�Aw�Av{At5?Ap��Ao`BAnjAln�AlbAj��Ah�RAgx�AfĜAf(�AeAe|�Ae?}Ad��Ac�Aat�A_��A]��A\��AY�hAW�AS��AQVAP(�AP9XAO��AO�;AOK�AN^5AM\)ALr�AJ  AGO�AE�AD1'ACXAAO�A?dZA="�A;��A:��A9�wA7��A5��A4ĜA3�7A3�A2��A1�FA0~�A.�/A-�#A+��A*�A*z�A)XA(A'�7A%��A$��A$^5A"�/A!�;A �`A`BA��AO�A�/AI�AE�AQ�A��A��A�A��AI�AȴA|�AbNA��AȴA1A�FAȴAQ�AS�A1'A7LA
1'A	�-A�RA~�AVA�FA�yAr�Ax�A�AVA�AS�A �D@��+@�ff@���@�
=@���@��@�@�|�@�S�@�K�@�S�@��y@���@�(�@��@��@睲@�5?@�@�u@�+@�^5@�I�@�o@�@ݺ^@�p�@܃@۝�@ڇ+@�hs@�Ĝ@׍P@֟�@�5?@ղ-@��/@�o@�V@�$�@�?}@ύP@�dZ@�"�@�-@�bN@�|�@�
=@��@�O�@�z�@�  @Ǯ@�dZ@��@�1@¸R@���@���@��@�v�@�G�@��/@�A�@�C�@�5?@�hs@�z�@�"�@�^5@���@���@��;@�\)@�+@�V@���@�@�hs@���@��D@�1'@��P@�l�@���@��!@�$�@��@�hs@�&�@�Z@���@�|�@��@��7@�@�33@�@�r�@���@�V@���@�  @��
@�K�@��y@��!@��y@���@�~�@�=q@���@���@���@��h@�X@���@��!@�C�@�ƨ@�Z@�j@�j@��9@��h@�M�@�^5@�$�@��@�/@��`@� �@��;@���@�33@��+@�{@���@�7L@��@���@�O�@�V@��/@�  @�$�@�%@�Z@� �@�9X@��@��!@��#@��#@�E�@�=q@�$�@�J@���@�1'@��m@��@���@��P@�S�@�"�@��@��\@�^5@�^5@�5?@�-@�J@�@���@���@��h@�X@�?}@�&�@��@�I�@��
@�l�@�"�@���@��@��!@��\@�n�@�M�@�J@���@��-@��@��@��/@�b@��m@��
@�ƨ@�1'@��@�\)@��@�=q@���@��h@�X@�?}@��@��`@��j@��@�1@���@�|�@�dZ@�S�@�
=@��H@���@�^5@�$�@��@��-@���@��7@�p�@�X@�O�@�7L@���@���@�Ĝ@��@��D@�Z@� �@��@��
@��F@��@��@�t�@�;d@���@��@���@��R@���@���@�~�@�ff@�V@��@��@�@��7@�&�G�O�@���@C@v�'@k�P@d�D@[�	@SC�@L��@D7@=��@6E�@.��@)c@$]d@�"@�2@�b@��@�m@v`@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB)�B)�B)�B)�B+B)�B)�B)�B)�B'�B�B{B	7B%B%B%B%BhB�B%�B&�B(�B$�B�BBBVB)�B8RBG�BT�BaHBk�Bv�B��B��B�B��B��B�B��B��B��B  B%BVB{BuB{B$�B,B/B>wBK�BP�BS�BYBffBn�Bq�Bq�Br�Bx�B� B�B�B�JB�=B�+B�1B�PB�=B�%B|�Be`BH�B5?B(�B �B�B�B�B0!B1'B,B$�B{B%B�TB�BȴBƨB�^B�B��B� Bq�BW
BC�B@�B=qB@�B7LB�BB
��B
�B
�BB
��B
�3B
�B
��B
�VB
x�B
bNB
F�B
_;B
s�B
s�B
cTB
W
B
K�B
B�B
9XB
,B
�B
1B
B	��B	�B	�sB	�ZB	�BB	�/B	�B	�B	��B	��B	��B	��B	�}B	�?B	�B	��B	�\B	�B	gmB	L�B	F�B	H�B	M�B	N�B	K�B	E�B	?}B	8RB	,B	�B	VB	%B	B��B�B�TB�B�
B��B��B�}B�?B�'B�-B�XB�FB�B��B��B��B��B��B��B�VB�=B�B}�Bw�Bq�BiyBdZBbNBk�BdZBe`BffBgmBiyBiyBhsBcTBe`BcTBbNB_;B]/BZBZBW
BW
BT�BR�BQ�BN�BN�BK�BK�BJ�BH�BH�BH�BF�BF�BD�BC�BB�B>wB>wB;dB<jB;dB;dB8RB7LB9XB9XB9XB:^B:^B:^B;dB=qBC�BB�BA�BA�BB�BB�BD�BF�BG�BK�BM�BO�BO�BO�BP�BQ�BS�BS�BT�BXBXBXBYBZB^5B^5B]/B`BBaHBaHBaHBcTBe`BffBgmBjBl�Bo�Bq�Br�Br�Bw�Bz�B{�B}�B� B�B�B�B�B�+B�=B�JB�JB�JB�VB�VB�bB�oB��B��B��B��B��B��B��B��B��B��B�'B�-B�?B�RB�jB�wB�wB�}BBǮB��B��B��B�;B�fB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	B	
=B	JB	\B	{B	�B	#�B	(�B	1'B	5?B	8RB	=qB	D�B	I�B	K�B	M�B	O�B	O�B	Q�B	W
B	YB	[#B	\)B	]/B	\)B	\)B	[#B	[#B	_;B	k�B	l�B	l�B	l�B	l�B	k�B	n�B	q�B	s�B	u�B	v�B	u�B	v�B	x�B	z�B	z�B	y�B	w�B	u�B	x�B	y�B	z�B	{�B	|�B	~�B	� B	�B	�B	�B	�%B	�%B	�+B	�7B	�=B	�=B	�DB	�PB	�VB	�VB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�9B	�LB	�FB	�LB	�LB	�LB	�LB	�XB	�XB	�XB	�XB	�XB	�^B	�jB	�wB	�wB	�wB	�wB	��B	��B	B	ÖB	ŢB	ƨB	ǮB	ȴB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�;B	�;B	�tB	�"B	��B
�B
�B
$B
-CB
7B
=�B
C�B
LB
Q�B
S�B
T�B
^�B
c�B
h�B
l�B
q�B
tB
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B"}B"}B"}B"}B#�B"}B"}B"}B"}B qB.B�B�B
��B
��B
��B
��B	�B/BfBlB!yB`BB
��B
��B�B"�B0�B@0BMBY�BdBoIB�B�UB��B��B�gB�B�NB�TB�UB�yB��B�B�B�B�BTB$B'�B6�BD=BIZBLmBQ�B^�BgBjBjBk$BqIBxtB|�B}�B��B��B�B��B��B��B~�BucB]�BA,B-�B!pB@BBB-B(�B)�B$�BXB�B��B��BјB�6B�*B��B��B�Bx�Bj2BO�B<!B9B5�B9B/�B4B
��B
�kB
�)B
��B
�B
��B
��B
�gB
��B
qnB
Z�B
?DB
W�B
lPB
lPB
[�B
O�B
DdB
;,B
1�B
$�B
3B
 �B	��B	�_B	�GB	�B	��B	��B	��B	ѼB	ΩB	̝B	ʒB	ȅB	�gB	�$B	��B	��B	�zB	�B	{�B	`B	E|B	?XB	AdB	F�B	G�B	DvB	>RB	8-B	1B	$�B	FB	
B��B��B��B�GB�B��B��B˫BņB�7B��B��B��B�B�B��B��B��B�uB�QB�QB�EB�B��B}�Bv�Bp�BjkBb:B]B[BdGB]B^"B_(B`/Bb;Bb;Ba5B\B^#B\B[BW�BU�BR�BR�BO�BO�BM�BK�BJ�BG�BG�BD�BD�BC�BAzBAzBAzB?oB?oB=cB<]B;VB7?B7?B4,B52B4,B4-B1B0B2!B2!B2!B3'B3'B3'B4-B6:B<_B;XB:RB:RB;XB;XB=eB?qB@wBD�BF�BH�BH�BH�BI�BJ�BL�BL�BM�BP�BP�BP�BQ�BR�BV�BV�BU�BYBZBZBZB\B^)B_/B`6BcHBeTBhgBjsBkyBkyBp�Bs�Bt�Bv�Bx�Bz�B}�B}�B}�B�B�B�B�B�B�B�B�*B�7B�IB�OB�[B�hB�hB�hB��B��B��B��B��B��B�B�B�0B�=B�=B�CB�UB�tBœBƙBɫB� B�*B�IB�CB�CB�CB�IB�\B�\B�B�B�B��B��B��B��B��B��B	 B	B	B	=B	nB	�B	!�B	)�B	-�B	1B	61B	=[B	ByB	D�B	F�B	H�B	H�B	J�B	O�B	Q�B	S�B	T�B	U�B	T�B	T�B	S�B	S�B	W�B	dBB	eHB	eHB	eHB	eIB	dCB	gVB	jgB	lsB	n�B	o�B	n�B	o�B	q�B	s�B	s�B	r�B	p�B	n�B	q�B	r�B	s�B	t�B	u�B	w�B	x�B	{�B	|�B	}�B	~�B	~�B	�B	��B	��B	��B	� B	�B	�B	�B	�+B	�7B	�=B	�VB	�bB	�bB	�hB	�nB	�tB	�tB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�$B	�1B	�1B	�1B	�1B	�=B	�CB	�IB	�PB	�\B	�bB	�hB	�nB	�nB	�tB	�tB	�tB	�{B	āB	ŇB	ƍB	ǓB	ȘB	ɞB	ʥB	ʥB	˫B	˫B	˫B	̱B	ͷB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	�-B	��B	�WB
|B
�B
�B
%�B
/�B
6�B
<eB
D�B
JRB
L^B
M�B
W�B
\�B
aAB
e?B
j�B
l�B
p411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.17 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.007(+/-0.005) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144192022020411441920220204114419  AO  ARCAADJP                                                                    20200619170913    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170913  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170913  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114419  IP                  G�O�G�O�G�O�                