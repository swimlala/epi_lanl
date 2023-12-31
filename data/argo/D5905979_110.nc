CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:19Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170919  20220204114422  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               nA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @���C�`1   @��o�y@5��;dZ�cbM��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    nA   B   B   @�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B7��B@  BH  BP  BX  B`  BhffBp  Bx  B�  B���B�  B�  B�33B�  B�  B�  B���B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�fC  C  C
  C  C  C  C  C  C  C  C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DUy�DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dgy�Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm�fDn  Dn� Do  Do� DpfDp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D�+�D�Z�D��3D�њD�(�D�W\D��=D��\D�#�D�T{D��qD��fD�!�D�a�Dړ3D��3D�)D�QHD�D��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�{@�{@��HA
=A;
=A[
=A{
=A��A��A��A��AͅA݅A�Q�A��BBBBB&B.B6\)B>BFBNBVB^Bg(�BnBvB~B�.B�aHB�aHB��{B�aHB�aHB�aHB�.B�aHB�aHB���B�.B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHC��C�
C��C��C	��C��C��C��C��C��C��C��C��C�
C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��RC��RC��RC��RC��RC��RC��RC�˅C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�˅C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD l)D �)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)D	l)D	�)D
l)D
�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)D l)D �)D!l)D!�)D"l)D"�)D#l)D#�)D$l)D$�)D%l)D%�)D&l)D&�)D'l)D'�)D(l)D(�)D)l)D)�)D*l)D*�)D+l)D+�)D,l)D,�)D-l)D-�)D.l)D.�)D/l)D/�)D0l)D0�)D1l)D1�)D2l)D2�)D3l)D3�)D4l)D4�)D5l)D5�)D6l)D6�)D7l)D7�)D8l)D8�)D9l)D9�)D:l)D:�)D;l)D;�)D<l)D<�)D=l)D=�)D>l)D>�)D?l)D?�)D@l)D@�)DAl)DA�)DBl)DB�)DCl)DC�)DDl)DD�)DEl)DE�)DFl)DF�)DGl)DG�)DHl)DH�)DIl)DI�)DJl)DJ�)DKl)DK�)DLl)DL�)DMl)DM�)DNl)DN�)DOl)DO�)DPl)DP�)DQl)DQ�)DRl)DR�)DSl)DS�)DTl)DT�)DUe�DU�)DVl)DV�)DWl)DW�)DXl)DX�)DYl)DY�)DZl)DZ�)D[l)D[�)D\l)D\�)D]l)D]�)D^l)D^�)D_l)D_�)D`l)D`�)Dal)Da�)Dbl)Db�)Dcl)Dc�)Ddl)Dd�)Del)De�)Dfl)Df�)Dge�Dg�)Dhl)Dh�)Dil)Di�)Djl)Dj�)Dkl)Dk�)Dll)Dl�)Dmr�Dm�)Dnl)Dn�)Dol)Do�Dpl)Dp�)Dql)Dq�)Drl)Dr�)Dsl)Ds�)Dtl)Dt��Dy�D�!�D�P�D��GD�ǮD��D�MpD��QD��pD��D�J�D���D��zD� D�W�DډGD�GD�=D�G\D�D��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�hsA�hsA�p�A�r�A�v�A�|�A؃A؇+A؋DA؉7A؏\A؏\A؏\AؑhAؑhAؓuAؗ�Aؗ�Aؗ�Aؗ�A؝�A؝�A؝�A؝�A؟�A؝�A؝�A؟�Aء�Aء�AؑhA�ZA֩�A��HA�ZA�{A�oA�ffAʡ�A�E�A��A��-A���A�t�A�x�A�A�5?A�ffA�x�A�9XA�1A�I�A���A�v�A�x�A�{A���A���A��A�hsA��A��A�K�A���A��TA��A�&�A�7LA��#A�K�A��A���A�ffA��yA��uA�I�A���A��DA�\)A��wA��A�/A��-A�\)A��HA��jA�hsA��;A�;dA�A��jA�33A�=qA��TA��A�r�A�r�A���A��A�&�A��A�O�A���A��A�+A�v�A�JA~A{��Az$�Au"�Ar�9Aq\)An(�Amt�Am;dAljAkx�Ag��Ae�#Ac�#A`�yA^�A^1'A\n�A[+AY%AWl�AU��ATE�APZAN�uAM��AM`BAM�#AM��AL�DAK�^AJ5?AIVAGXAFĜAE�wADffAC�hAB�jABI�A@ZA>ZA=��A=l�A;t�A:JA8(�A6�A5XA4r�A2�RA1+A/�;A/33A-A,5?A+�A)G�A(��A(A�A'��A%��A$�A$ffA$bA#S�A"z�A ȴA�^A��A5?A&�A5?A��A��AM�A�hA��An�A�AJA��A�A�PA+A��A�A�A��AC�A�Av�A�wA%A	�^A	A5?AhsA�A�AO�AC�A��A{A/A�\A��AG�A z�@���@��H@�~�@�E�@�{@�G�@�l�@���@��u@��!@��-@���@�\)@��@�u@�!@��@���@��@���@�-@�@�V@��@߶F@�  @�$�@�dZ@ٺ^@��;@�ȴ@ղ-@�j@�K�@�
=@�M�@ЋD@ύP@�j@��m@�dZ@�E�@��`@�%@��@�$�@��@ě�@�b@�\)@\@+@¸R@\@���@��w@��-@�hs@�?}@�&�@���@��-@�%@�`B@�A�@��@���@���@��@���@���@�dZ@���@�p�@��j@��@��+@���@��@��@�@���@�ff@�
=@���@���@��7@���@��7@�O�@�7L@�r�@�bN@�I�@�1@� �@��;@���@�\)@��w@� �@��+@��^@�`B@��@���@���@��w@�M�@�x�@��9@�1@���@���@���@���@�C�@���@���@�t�@��D@�hs@���@�j@���@�33@��\@��-@��@�~�@���@��-@���@�E�@��@�33@�5?@�&�@���@��@���@���@��!@�^5@�=q@�5?@�@�@��T@��@�O�@���@��@��@�p�@�`B@�O�@�%@���@���@�Ĝ@��j@��9@�bN@�b@��m@���@�\)@�S�@�K�@�+@���@���@��\@�E�@�-@��@�V@�^5@�n�@���@�^5@�ff@�5?@��^@���@���@�X@�%@��@�A�@�(�@�(�@�9X@� �@��@�1@���@��P@�S�@�+@�@���@��\@�V@�V@�E�@�-@�J@��#@���@�X@�/@��@���@�r�@�bN@�1'@���@��;@���@�|�@�t�@�t�@�t�@�|�@�t�@�"�@�@��y@���@�n�@�M�@�5?@��@��#@��^@���@��-@���@�7L@�%@���@���@�j@�1'@�1@��F@�|�@�\)@�K�@�33@���@�ȴ@��!@��\@�v�@�ff@�=q@���@��T@���@���@��@�%@�Ĝ@�bN@�Q�@�9X@� �@�  @|�@~�y@{8@uj@n�@e�>@_�@W��@P��@K�
@E(�@=��@7��@0�$@,�@'W?@!�@Ĝ@tT@-�@��@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�hsA�hsA�p�A�r�A�v�A�|�A؃A؇+A؋DA؉7A؏\A؏\A؏\AؑhAؑhAؓuAؗ�Aؗ�Aؗ�Aؗ�A؝�A؝�A؝�A؝�A؟�A؝�A؝�A؟�Aء�Aء�AؑhA�ZA֩�A��HA�ZA�{A�oA�ffAʡ�A�E�A��A��-A���A�t�A�x�A�A�5?A�ffA�x�A�9XA�1A�I�A���A�v�A�x�A�{A���A���A��A�hsA��A��A�K�A���A��TA��A�&�A�7LA��#A�K�A��A���A�ffA��yA��uA�I�A���A��DA�\)A��wA��A�/A��-A�\)A��HA��jA�hsA��;A�;dA�A��jA�33A�=qA��TA��A�r�A�r�A���A��A�&�A��A�O�A���A��A�+A�v�A�JA~A{��Az$�Au"�Ar�9Aq\)An(�Amt�Am;dAljAkx�Ag��Ae�#Ac�#A`�yA^�A^1'A\n�A[+AY%AWl�AU��ATE�APZAN�uAM��AM`BAM�#AM��AL�DAK�^AJ5?AIVAGXAFĜAE�wADffAC�hAB�jABI�A@ZA>ZA=��A=l�A;t�A:JA8(�A6�A5XA4r�A2�RA1+A/�;A/33A-A,5?A+�A)G�A(��A(A�A'��A%��A$�A$ffA$bA#S�A"z�A ȴA�^A��A5?A&�A5?A��A��AM�A�hA��An�A�AJA��A�A�PA+A��A�A�A��AC�A�Av�A�wA%A	�^A	A5?AhsA�A�AO�AC�A��A{A/A�\A��AG�A z�@���@��H@�~�@�E�@�{@�G�@�l�@���@��u@��!@��-@���@�\)@��@�u@�!@��@���@��@���@�-@�@�V@��@߶F@�  @�$�@�dZ@ٺ^@��;@�ȴ@ղ-@�j@�K�@�
=@�M�@ЋD@ύP@�j@��m@�dZ@�E�@��`@�%@��@�$�@��@ě�@�b@�\)@\@+@¸R@\@���@��w@��-@�hs@�?}@�&�@���@��-@�%@�`B@�A�@��@���@���@��@���@���@�dZ@���@�p�@��j@��@��+@���@��@��@�@���@�ff@�
=@���@���@��7@���@��7@�O�@�7L@�r�@�bN@�I�@�1@� �@��;@���@�\)@��w@� �@��+@��^@�`B@��@���@���@��w@�M�@�x�@��9@�1@���@���@���@���@�C�@���@���@�t�@��D@�hs@���@�j@���@�33@��\@��-@��@�~�@���@��-@���@�E�@��@�33@�5?@�&�@���@��@���@���@��!@�^5@�=q@�5?@�@�@��T@��@�O�@���@��@��@�p�@�`B@�O�@�%@���@���@�Ĝ@��j@��9@�bN@�b@��m@���@�\)@�S�@�K�@�+@���@���@��\@�E�@�-@��@�V@�^5@�n�@���@�^5@�ff@�5?@��^@���@���@�X@�%@��@�A�@�(�@�(�@�9X@� �@��@�1@���@��P@�S�@�+@�@���@��\@�V@�V@�E�@�-@�J@��#@���@�X@�/@��@���@�r�@�bN@�1'@���@��;@���@�|�@�t�@�t�@�t�@�|�@�t�@�"�@�@��y@���@�n�@�M�@�5?@��@��#@��^@���@��-@���@�7L@�%@���@���@�j@�1'@�1@��F@�|�@�\)@�K�@�33@���@�ȴ@��!@��\@�v�@�ff@�=q@���@��T@���@���@��@�%@�Ĝ@�bN@�Q�@�9X@� �@�  @|�G�O�@{8@uj@n�@e�>@_�@W��@P��@K�
@E(�@=��@7��@0�$@,�@'W?@!�@Ĝ@tT@-�@��@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
ɺB
ɺB
ȴB
ɺB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ɺB
ȴB
ɺB
��B
�5BVBB�BS�Br�BjBbNB[#BP�BK�BXBp�B�RBBD�B`BBgmBp�Bm�Br�Be`B;dB�B�5B�BɺBǮB�^B��B�Bo�Bl�Br�Br�Bs�B��B&�B"�B�ZB  B
=BbB�B�B�BbB
=B��B  B��B�fB�)B��B��BȴBƨB�}B�-Bu�BYBO�BD�B.BuB
��B
�5B
��B
ŢB
��B
��B
�3B
�DB
D�B
B�B
]/B
L�B
33B
�B
PB	��B	�HB	ɺB	ÖB	�'B	��B	��B	��B	��B	�DB	� B	t�B	e`B	S�B	O�B	K�B	O�B	E�B	=qB	5?B	'�B��B�NB�;B�ZB��B	B��B	+B��B�B�NB�sB�B�B�B�mB�NB�BȴB��B��B��B�9B�B��B��B��B�VB�PB�=B�=B�7B�%B�B}�B{�B|�B}�B|�B{�By�Bx�Bv�Bt�Br�Bo�Bl�Bl�BiyBjBl�Bs�Br�Bs�Bq�Bp�Bn�Bl�Bl�Bk�BjBk�Bk�Bn�BjBffBe`BaHB]/B]/B\)BT�BR�BQ�BO�BO�BL�BI�BI�BH�BF�BE�BD�BB�BB�BE�BF�BG�BJ�BM�BP�BP�BQ�BO�BM�BK�BI�BG�BD�BA�B?}B<jB;dB9XB8RB6FB8RB<jB9XB=qB@�BE�BD�B>wB<jB=qB=qBA�BI�BI�BO�BR�BS�B[#BT�BR�BP�BN�BI�BP�BL�BJ�BJ�BK�BL�BN�BO�BS�BW
BZB]/B_;B[#B\)B]/BcTBk�BjBiyBp�Bo�By�B�B}�Bt�Bt�B�By�Bv�Bw�Bv�B|�B~�B�B�%B�7B�hB�oB�{B��B��B��B��B��B��B�B�-B�9B�3B�9B�?B�RB�dB�wBBɺB��B��B��B�5B�)B�fB�mB�B�B�B�B�B��B��B	B	
=B	JB	JB	
=B	DB	oB	�B	�B	�B	�B	�B	�B	�B	{B	VB	PB	bB	{B	�B	�B	�B	 �B	 �B	�B	%�B	)�B	1'B	49B	9XB	;dB	;dB	<jB	=qB	A�B	C�B	C�B	G�B	N�B	N�B	P�B	R�B	W
B	XB	ZB	]/B	]/B	^5B	_;B	bNB	ffB	hsB	k�B	m�B	n�B	q�B	t�B	t�B	t�B	t�B	t�B	u�B	x�B	{�B	|�B	}�B	�B	�B	�1B	�=B	�1B	�7B	�DB	�VB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�?B	�FB	�RB	�XB	�dB	�}B	��B	��B	B	ĜB	ŢB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�/B	�/B	�5B	�;B	�BB	�BB	�HB	�HB	�NB	�TB	�TB	�ZB	�`B	�`B	�fB	�fB	�mB	�fB	�mB	�mB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B
�B
 B
�B
%B
./B
88B
<PB
BB
I�B
NB
T,B
X�B
Z�B
e�B
k�B
l�B
r-B
uZB
x�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�<B
�<B
�6B
�<B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�<B
�6B
�<B
�BB
ӵB�B8BIoBh&B_�BW�BP�BF_BABBM�BfB��B��B:BU�B\�BfBb�BhBZ�B0�B(BӪB΍B�2B�&B��B�/Bx�BeBbBh1Bh1Bi8B�fB\BEB��B�wB��B�BB	B
�B�B��B�TB�xB�<B��BѥB�bB�PB�2B�&B��B��BkHBN�BEhB:&B#�B	B
�wB
��B
�bB
�8B
�B
�4B
��B
��B
:>B
81B
R�B
BoB
(�B
dB
�B	��B	��B	�iB	�FB	��B	��B	��B	�~B	�ZB	��B	u�B	jtB	[B	I�B	E�B	A�B	E�B	;_B	3/B	*�B	�B�B�B�B�!B�B��B�B��B�B�RB�B�;B�GB�qB�lB�5B�B��B��B��BB�VB�B��B��B�dB�RB�(B�#B�B�BB{�Bx�Bs�Bq�Br�Bs�Br�Bq�Bo�Bn�Bl�Bj�Bh�BevBbcBbdB_RB`XBbdBi�Bh�Bi�Bg�Bf}BdrBbeBbeBa_B`ZBa`Ba`BdsB`ZB\BB[<BW$BSBSBRBJ�BH�BG�BE�BE�BB�B?�B?�B>�B<�B;�B:}B8pB8pB;�B<�B=�B@�BC�BF�BF�BG�BE�BC�BA�B?�B=�B:B7lB5`B2NB1HB/<B.7B,+B.7B2OB/=B3VB6hB;�B:�B4\B2PB3WB3WB7oB?�B?�BE�BH�BI�BQBJ�BH�BF�BD�B?�BF�BB�B@�B@�BA�BB�BD�BE�BI�BL�BPBSBU!BQ	BRBSBY:BajB`dB__Bf�Be�Bo�Bv�Bs�Bj�Bj�Bw�Bo�Bl�Bm�Bl�Br�Bt�Bv�B|
BB�LB�SB�_B��B��B��B��B��B��B��B�B�B�B�B� B�3B�EB�XB�oB��B��B��B��B�B�B�DB�KB�iB�vB�B�|B�B�B��B��B	 B	&B	&B	 B	 B	JB	uB	uB	{B	�B	�B	�B	hB	
VB	2B	,B	>B	
WB	cB	{B	�B	�B	�B	�B	�B	�B	'B	*B	/1B	1=B	1=B	2CB	3JB	7bB	9oB	9oB	=�B	D�B	D�B	F�B	H�B	L�B	M�B	O�B	SB	SB	TB	UB	X$B	\<B	^IB	a[B	cfB	dmB	gB	j�B	j�B	j�B	j�B	j�B	k�B	n�B	q�B	r�B	s�B	w�B	y�B	~B	�B	~B	
B	�B	�)B	�/B	�;B	�HB	�TB	�eB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�"B	�(B	�4B	�MB	�SB	�XB	�^B	�kB	�qB	��B	��B	��B	��B	��B	��B	B	ŭB	ƳB	ǺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�-B	�-B	�3B	�3B	�:B	�3B	�:B	�:B	�@B	�FB	�FB	�FB	�FB	�FB	�LB	�LG�O�B	�NB	��B
�B
�B
�B
#�B
.B
2B
7�B
?hB
C�B
I�B
NvB
P�B
[\B
afB
b�B
g�B
k B
nLB
q_111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.31 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.01(+/-0.004) in PSS-78.                                                                                                                                                                                        Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144222022020411442220220204114422  AO  ARCAADJP                                                                    20200619170919    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170919  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170919  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114422  IP                  G�O�G�O�G�O�                