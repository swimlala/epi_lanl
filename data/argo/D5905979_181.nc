CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-10-17T14:00:54Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20201017140054  20220204114429  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @�@^M���1   @�@^��@5���+�bٲ-V1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP��BW��B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl�Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#�fD$fD$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]y�D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy��D�"�D�O
D��=D��D�*�D�d)D���D��\D��D�^�D��)DǱ�D�'\D�NDګ3D�� D��D�\�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�
>@�=q@�=qA�A=�A[�A}�A��\A��\A��\A��\AΏ\Aޏ\A�\A��\BG�BG�BG�BG�B'G�B/G�B7G�B?G�BGG�BP{BV�HB^�HBgG�BoG�BwG�BG�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bã�Bǣ�Bˣ�Bϣ�Bӣ�Bף�Bۣ�Bߣ�B��B��B��B��B��B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg�Ci��Ck�Cm�Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�DnD�{Dt{D�{D	t{D	�{D
t{D
�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D t{D �{D!t{D!�{D"t{D"�{D#z�D#��D$t{D$�{D%t{D%�{D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*t{D*�{D+t{D+�{D,t{D,�{D-t{D-�{D.t{D.�{D/t{D/�{D0t{D0�{D1t{D1�{D2t{D2�{D3t{D3�{D4t{D4�{D5t{D5�{D6t{D6�{D7t{D7�{D8t{D8�{D9t{D9�{D:t{D:�{D;t{D;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�{D@t{D@�{DAt{DA�{DBt{DB�{DCt{DC�{DDt{DD�{DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�{DIt{DI�{DJt{DJ�{DKt{DK�{DLt{DL�{DMt{DM�{DNt{DN�{DOt{DO�{DPt{DP�{DQt{DQ�{DRt{DR�{DSt{DS�{DTt{DT�{DUt{DU�{DVt{DV�{DWt{DW�{DXt{DX�{DYt{DY�{DZt{DZ�{D[t{D[�{D\t{D\�{D]nD]�{D^t{D^�{D_t{D_�{D`t{D`�{Dat{Da�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{Det{De�{Dft{Df�{Dgt{Dg�{Dht{Dh�{Dit{Di�{Djt{Dj�{Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnt{Dn�{Dot{Do�{Dpt{Dp�{Dqt{Dq�{Drt{Dr�{Dst{Ds�{Dtt{Dt�Dy�qD��D�IGD��zD��\D�%D�^fD��)D��D��D�X�D��fDǬ)D�!�D�HQDڥpD��=D��D�W
D�\D��
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�$�A�&�A�"�A�&�A�-A�5?A�33A�5?A�9XA�7LA�9XA�9XA�9XA�9XA�r�AҲ-A�jAѼjA�%A��HAЏ\A�I�A���A�v�A��AʸRAȑhA��A�$�A��FA�bA���A�ȴA�A�7LA��`A���A�bNA���A��A�jA���A�v�A�(�A��A��yA�ffA�ƨA�ffA���A�"�A���A��A�E�A��^A�A�p�A��TA�~�A�/A���A��FA�A��A��A�ȴA���A���A��A�?}A�E�A�;dA�oA�bA�"�A��hA��A�C�A�+A��A�dZA���A�S�A�;dA��A�oA��uA�Q�A��HA���A��A��A��A�dZA���A�=qA��A���A���A��A�A��A��A�VA�-A���A��/A�K�A��yA��hA�M�A���A�5?A��A~1'Ay��Au;dAqG�Ao�Aml�Ah��Ag�Ad�A_�A\�AZ��AXbATĜAS|�ASAP��AN��AKx�AH-AF�!AF  AD��AA�wA@-A?��A>�`A>v�A=;dA;S�A: �A8��A6��A5�wA4��A3\)A1�TA0��A/�#A/K�A-`BA,JA+dZA*ZA)K�A(�A'hsA&��A%�^A$n�A#O�A"�A!�A �uA �A�A��AJA��A�An�A��A�9A��A"�A?}A�#A�A�A��A�A��AĜA�jA��A�hAI�A�Ar�A�7A��AffA��A��A
=A
Q�A	��A
(�A
1A��AȴAE�A|�A�uA�A�wA�uA��A�A ��A A�A {@�ƨ@�o@��@��9@�Z@��@��R@�t�@��@���@�X@��j@�1'@�C�@�n�@�%@�1@��@�v�@�Z@��@�K�@�~�@�`B@��@���@�{@��`@��H@�?}@�33@�5?@�V@ԓu@�33@�ȴ@�v�@�@ύP@�@̬@�(�@�t�@���@�M�@��m@�-@�M�@�5?@�?}@���@��@��F@�{@�@�O�@���@�j@�l�@���@�@���@�1@�b@��@�@���@�M�@�/@�K�@��H@�~�@�`B@��@���@���@�K�@��H@�ff@���@���@�z�@���@��@��y@��R@��+@��+@�V@��-@���@��@�Q�@� �@�K�@��@�n�@�{@��@���@��^@�p�@�x�@�/@�X@�?}@�9X@��!@��!@�M�@�$�@���@�G�@��/@�A�@��@�\)@��y@�=q@��-@�hs@��@��@�V@���@��/@�Ĝ@���@��D@��@��;@��m@��@��@�dZ@�+@��@��@�v�@�$�@��@�O�@�/@���@���@�bN@�1'@���@��
@���@�l�@�S�@�33@�"�@�@��H@���@��!@�v�@�n�@�=q@���@��-@�x�@��@���@�r�@�1'@�  @���@��@��@���@���@�|�@�ƨ@���@���@���@�S�@�ȴ@�M�@�{@��@�@���@�x�@�?}@�V@��j@�z�@�I�@��@�9X@�Z@��@��@��w@��@�l�@�33@�+@��@��@��!@���@�n�@�n�@�E�@��@���@�M�@��\@�$�@�@�p�@��h@��y@��y@�dZ@�;d@���@��^@��7@�G�@��D@�9X@�  @�t�@�o@�ƨ@�  @�I�@�Q�@�(�@��@��@�;d@�o@��!@��+@�v�@�5?@�J@��#@��-@�`B@�&�@�Ĝ@�Q�@��
@�+@��+@�v�@�=q@�@���@���@�x�@�O�@�?}@��@�%@��`@��/@��9@�r�@��u@�j@��
@�l�@�o@��@�}�@{˒@th�@kK�@ak�@Z�+@Q;@K�[@C�@>h
@6��@2@+�+@'�@!+@*0@�u@j@��@�A@
=q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�$�A�&�A�"�A�&�A�-A�5?A�33A�5?A�9XA�7LA�9XA�9XA�9XA�9XA�r�AҲ-A�jAѼjA�%A��HAЏ\A�I�A���A�v�A��AʸRAȑhA��A�$�A��FA�bA���A�ȴA�A�7LA��`A���A�bNA���A��A�jA���A�v�A�(�A��A��yA�ffA�ƨA�ffA���A�"�A���A��A�E�A��^A�A�p�A��TA�~�A�/A���A��FA�A��A��A�ȴA���A���A��A�?}A�E�A�;dA�oA�bA�"�A��hA��A�C�A�+A��A�dZA���A�S�A�;dA��A�oA��uA�Q�A��HA���A��A��A��A�dZA���A�=qA��A���A���A��A�A��A��A�VA�-A���A��/A�K�A��yA��hA�M�A���A�5?A��A~1'Ay��Au;dAqG�Ao�Aml�Ah��Ag�Ad�A_�A\�AZ��AXbATĜAS|�ASAP��AN��AKx�AH-AF�!AF  AD��AA�wA@-A?��A>�`A>v�A=;dA;S�A: �A8��A6��A5�wA4��A3\)A1�TA0��A/�#A/K�A-`BA,JA+dZA*ZA)K�A(�A'hsA&��A%�^A$n�A#O�A"�A!�A �uA �A�A��AJA��A�An�A��A�9A��A"�A?}A�#A�A�A��A�A��AĜA�jA��A�hAI�A�Ar�A�7A��AffA��A��A
=A
Q�A	��A
(�A
1A��AȴAE�A|�A�uA�A�wA�uA��A�A ��A A�A {@�ƨ@�o@��@��9@�Z@��@��R@�t�@��@���@�X@��j@�1'@�C�@�n�@�%@�1@��@�v�@�Z@��@�K�@�~�@�`B@��@���@�{@��`@��H@�?}@�33@�5?@�V@ԓu@�33@�ȴ@�v�@�@ύP@�@̬@�(�@�t�@���@�M�@��m@�-@�M�@�5?@�?}@���@��@��F@�{@�@�O�@���@�j@�l�@���@�@���@�1@�b@��@�@���@�M�@�/@�K�@��H@�~�@�`B@��@���@���@�K�@��H@�ff@���@���@�z�@���@��@��y@��R@��+@��+@�V@��-@���@��@�Q�@� �@�K�@��@�n�@�{@��@���@��^@�p�@�x�@�/@�X@�?}@�9X@��!@��!@�M�@�$�@���@�G�@��/@�A�@��@�\)@��y@�=q@��-@�hs@��@��@�V@���@��/@�Ĝ@���@��D@��@��;@��m@��@��@�dZ@�+@��@��@�v�@�$�@��@�O�@�/@���@���@�bN@�1'@���@��
@���@�l�@�S�@�33@�"�@�@��H@���@��!@�v�@�n�@�=q@���@��-@�x�@��@���@�r�@�1'@�  @���@��@��@���@���@�|�@�ƨ@���@���@���@�S�@�ȴ@�M�@�{@��@�@���@�x�@�?}@�V@��j@�z�@�I�@��@�9X@�Z@��@��@��w@��@�l�@�33@�+@��@��@��!@���@�n�@�n�@�E�@��@���@�M�@��\@�$�@�@�p�@��h@��y@��y@�dZ@�;d@���@��^@��7@�G�@��D@�9X@�  @�t�@�o@�ƨ@�  @�I�@�Q�@�(�@��@��@�;d@�o@��!@��+@�v�@�5?@�J@��#@��-@�`B@�&�@�Ĝ@�Q�@��
@�+@��+@�v�@�=q@�@���@���@�x�@�O�@�?}@��@�%@��`@��/@��9@�r�@��u@�j@��
@�l�@�oG�O�@�}�@{˒@th�@kK�@ak�@Z�+@Q;@K�[@C�@>h
@6��@2@+�+@'�@!+@*0@�u@j@��@�A@
=q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB@�B@�B@�B@�B@�B?}B@�B@�B?}B@�B@�B@�B?}B>wB?}B=qB8RB+B�B�B�B �B�B�BbB	7BB	7B	7BJB	7BDB�B33B@�BJ�BXB[#BdZBgmB}�B�B�\B��B��B��B��B�B�!B�wB��B��B�#B�mB�B��B��BDB�B-B1'B7LB8RBK�B?}B<jBYB`BB\)BR�BG�B9XB!�B�B{BDB�B	7B%BB��B�B�B��BÖBÖB�jB�RB�!B��B�\B|�Bq�BiyB_;BXBL�B6FB!�BoBVB  B
��B
�B
�B
��B
ÖB
�?B
�B
��B
�B
v�B
e`B
I�B
33B
bB	�B	��B	��B	�'B	�uB	�B	s�B	N�B	:^B	.B	!�B	VB	B��B�B�fB�/BǮB��B�dB�?B�B��B��B��B��B��B�bB�7B�%B}�By�Bv�Br�Bp�Bl�BjBgmBe`B`BBaHBbNB_;B^5B[#B[#BZBYBW
BT�BR�BP�BN�BK�BF�BB�B>wB<jB9XB<jB=qB;dB;dBE�BI�BL�BL�BYBhsBl�Bm�Bq�Bn�B\)BP�BR�BZBW
BN�BN�BL�BO�BQ�BXBW
BbNBiyBhsB\)BZB`BBaHBhsBiyBgmBgmBiyBk�Bk�BiyBhsBffBgmBm�Br�Bs�By�Bt�Bx�B� B�%B�DB�PB�PB�VB�\B�hB�hB�\B�hB�\B�=B�7B�7B�VB�bB�oB��B��B�uB�JB�=B�+B�+B�%B�1B�DB�=B�JB�DB�DB�DB�DB�JB�JB�JB�7B�bB�oB��B��B��B�\B�bB��B��B��B��B��B��B��B��B��B�B�'B�3B�?B�?B�^B�}B�}BBŢBƨBȴB��B��B��B��B��B��B��B�B�B�B�#B�BB�HB�NB�TB�TB�TB�NB�`B�B�B�B�B�B�B�B��B��B	  B	B	+B		7B	+B	VB	hB	hB	oB	�B	�B	�B	#�B	&�B	'�B	+B	.B	5?B	7LB	7LB	7LB	8RB	9XB	<jB	>wB	@�B	D�B	G�B	J�B	N�B	Q�B	T�B	W
B	XB	\)B	\)B	^5B	cTB	dZB	gmB	jB	k�B	n�B	o�B	q�B	s�B	x�B	{�B	{�B	|�B	|�B	|�B	|�B	|�B	|�B	}�B	~�B	�B	�B	�B	�%B	�7B	�VB	�bB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�FB	�RB	�XB	�^B	�XB	�XB	�^B	�jB	�qB	�wB	�}B	�}B	��B	��B	ÖB	ĜB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	�
B	�#B	�;B	�;B	�BB	�BB	�;B	�BB	�BB	�BB	�HB	�;B	�;B	�`B	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
	7B
B
B
+�B
2�B
<�B
A�B
H�B
M6B
S�B
YB
]�B
b�B
g�B
k�B
q[B
u�B
x�B
{0B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B00B00B00B00B00B/*B00B00B/*B00B00B00B/*B.$B/*B-B( B�BdBdBpBwBdB	MB B
��B
��B
��B
��B
�B
��B
��BoB"�B07B:tBG�BJ�BT
BWBm�Bs�BB�2B�2B�EB�\B��B��B�B�lB��B��B�B�LB�B�B��B+B�B �B&�B'�B;_B/B,BH�BO�BK�BB�B7IB(�BlB$BB��B$B��B��B�B�B�:B��B�uB�EB�FB�B�B��B�[BBl�BaeBY6BN�BG�B<�B&
B�B8B
� B
��B
�B
�TB
��B
��B
�kB
�B
��B
��B
q�B
f�B
UBB
9�B
#B
 QB	�B	��B	�}B	�$B	�vB	sB	c�B	>�B	*mB	$B	�B�kB�)B�B��BւB�LB��B��B��B�cB�-B��B��B��B��B��B��BybBvQBn!Bj	Bf�Bb�B`�B\�BZ�BW�BU�BPuBQ{BR�BOoBNiBKXBKXBJRBIMBG@BE5BC)BAB?B< B6�B2�B.�B,�B)�B,�B-�B+�B+�B5�B9�B=B=BIQBX�B\�B]�Ba�B^�BLcBA!BC.BJXBGEB?B?B=
B@BB)BHLBGFBR�BY�BX�BLeBJZBP~BQ�BX�BY�BW�BW�BY�B[�B[�BY�BX�BV�BW�B]�Bb�Bc�BjBd�BiBp;Bv_B{~B}�B}�B~�B�B��B��B�B��B�BzyBysBysB~�B��B��B��B��B��B|�Bz{BwiBwiBvdBxoB{�Bz{B|�B{�B{�B{�B{�B|�B|�B|�BywB��B��B��B��B��B�B��B��B��B�B�!B�3B�3B�4B�4B�4B�EB�dB�pB�|B�|B��B��B��B��B��B��B��B��B��B�B�B�B�&B�2B�>B�JB�QB�]B�{BсB҇BӍBӍBӍB҇BՙBڷB۽B��B��B��B��B��B��B�B�6B�NB�`B�lB�`B��B	�B	�B	�B	�B	�B	�B		B	B	"B	4B	EB	%oB	'|B	'|B	'|B	(�B	)�B	,�B	.�B	0�B	4�B	7�B	:�B	?B	BB	E+B	G7B	H=B	LUB	LUB	NaB	SB	T�B	W�B	Z�B	[�B	^�B	_�B	a�B	c�B	h�B	lB	lB	mB	mB	mB	mB	mB	mB	nB	o"B	q.B	s:B	uGB	vMB	y^B	~}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	� B	�1B	�2B	�2B	�2B	�7B	�DB	�PB	�VB	�iB	�tB	�zB	��B	�zB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�)B	�AB	�YB	�YB	�`B	�`B	�YB	�`B	�`B	�`B	�fB	�YB	�YB	�}B	׊B	ڜB	ۢB	ܧB	ݭB	޴B	ߺB	��B	ߺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	߻B	޵B	ܨB	ڜB	ۢB	ۢB	ܨB	ܨB	ݮB	߻B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	�B	�RB
8B
 B
�B
"�B
,�B
1�B
8�B
=MB
C�B
I-B
M�B
R�B
W�B
[�B
apB
e�B
h�B
kDB
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.18 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.016(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144292022020411442920220204114429  AO  ARCAADJP                                                                    20201017140054    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20201017140054  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20201017140054  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114429  IP                  G�O�G�O�G�O�                