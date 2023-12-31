CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:09Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170909  20220204114417  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               @A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @خ���y1   @خI��V@6��+�dQ��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    @A   B   B   @���@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+�fD,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DS��DT� DU  DU� DV  DV� DW  DW� DX  DX� DYfDY�fDZ  DZy�D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� DhfDh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�qD�
=D�c�D���D�� D�{D�Q�D��D�ӅD�
D�S3D��\D�ۅD�D�Q�DڥqD��RD�fD�EqD�D�l)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��H@�{@�{A
=A9p�A[
=A{
=A��A��A��A��AͅA݅A�A��BBBBB&B.B6B>BFBNBVB^BfBnBvB~B�aHB�aHB�aHB�aHB�.B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_�
Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RD l)D �)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)D	l)D	�)D
l)D
�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)D l)D �)D!l)D!�)D"l)D"�)D#l)D#�)D$l)D$�)D%l)D%�)D&l)D&�)D'l)D'�)D(l)D(�)D)l)D)�)D*l)D*�)D+r�D+�)D,l)D,�)D-l)D-�)D.l)D.�)D/l)D/�)D0l)D0�)D1l)D1�)D2l)D2�)D3l)D3�)D4l)D4�)D5l)D5�)D6l)D6�)D7l)D7�)D8l)D8�)D9l)D9�)D:l)D:�)D;l)D;�)D<l)D<�)D=l)D=�)D>l)D>�)D?l)D?�)D@l)D@�)DAl)DA�)DBl)DB�)DCl)DC�)DDl)DD�)DEl)DE�)DFl)DF�)DGl)DG�)DHl)DH�)DIl)DI�)DJl)DJ�)DKl)DK�)DLl)DL�)DMl)DM�)DNl)DN�)DOl)DO�)DPl)DP�)DQl)DQ�)DRl)DR�)DSl)DS��DTl)DT�)DUl)DU�)DVl)DV�)DWl)DW�)DXl)DX�DYr�DY�)DZe�DZ�)D[l)D[�)D\l)D\�)D]l)D]�)D^l)D^�)D_l)D_�)D`l)D`�)Dal)Da�)Dbl)Db�)Dcl)Dc�)Ddl)Dd�)Del)De�)Dfl)Df�)Dgl)Dg�Dhl)Dh�)Dil)Di�)Djl)Dj�)Dkl)Dk�)Dll)Dl�)Dml)Dm�)Dnl)Dn�)Dol)Do�)Dpl)Dp�)Dql)Dq�)Drl)Dr�)Dsl)Ds�)Dtl)DtҏDy��D� QD�Y�D���D��D��D�G�D�{3D�əD�D�IGD��pD�љD�(D�G�Dڛ�D��fD�zD�;�D��D�b=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��/A��A��A��A��/A��/A��HA��HA��HA��HA��;A��;A��;A��/A��/A��HA��;A��;A��;A��HA��HA��HA��`A��`A��yA��A��`A��^A�oA���A���A�p�A�`BA�ZA�`BA�\)A�VA�5?A�(�A�(�A�"�A��A��A��A��A�bA�VA�JA�%A�1A�
=A�1A�A��A�ȴA�z�A�S�A�~�A���A��;A�;dA�ffA�oA�l�A�hsA��RA�bNA�l�A��
A���A�ƨA�ĜA�O�A��A��;A�7LA�5?A���A��PA���A�"�A��;A�^5A��A��/A�G�A��A��A��!A��A�\)A�z�A�oA��A�+A���A�r�A���A��A�~�A��A��#A��hA�$�A� �A��A?}A~��A~E�Az5?Aql�Am�
Al��AlI�Ak��Aj��Ai��Ah��Ag�#Af��A_��A]�A\1'A[��AZ�/AX�\AT��AR��AQ�;APȴAM��AKAIt�AH~�AHAF�yAC��A?l�A;A:�jA:z�A:(�A9�7A8��A733A6n�A5�A5�FA5x�A5"�A4�A4��A4n�A4I�A4A�A3�A2r�A1��A0�jA.^5A-�-A-/A,�HA,  A*�A)�#A(��A(v�A'�;A'�A&��A&-A%;dA$��A#�;A"Q�A!��A �\A7LA�A��A�yAA;dA�DA��AȴA��A��A7LA �A;dA��A �A	�-A5?A\)A�;AoA�mA/AĜAJA ^5@�M�@���@��@���@���@��;@�{@�`B@��u@��m@�l�@�@�-@��@�%@�r�@���@�"�@홚@���@�7@�o@�{@噚@�@���@㕁@�|�@�\)@�;d@�o@��y@�ȴ@⟾@�V@�@��u@߶F@��H@���@�@�=q@�O�@�l�@�hs@�&�@��@��@��;@�C�@��y@ҟ�@�^5@�x�@�I�@�K�@�{@�V@�o@�Ĝ@�Q�@Ǿw@�@��
@�x�@��@�S�@�^5@�J@���@�&�@��u@���@�33@���@�5?@�?}@�b@�33@�n�@��T@���@��@���@� �@���@���@�x�@�p�@�X@�Ĝ@�9X@���@��!@�M�@�hs@��@��@�bN@�ƨ@�S�@��P@�+@�E�@��@���@�/@��u@�bN@���@��P@�33@�o@���@���@��#@�`B@�&�@���@���@��@��@�A�@��m@�S�@��H@�V@���@��^@��@�O�@��@��@�I�@�b@��
@�o@��!@���@�G�@�/@��@��@�r�@�(�@�  @�ƨ@�|�@��H@��R@���@��+@�-@�x�@��@��@���@�r�@�A�@��@���@�t�@�dZ@�;d@�
=@��y@���@�@�hs@��9@��P@�"�@��@�{@��#@���@��@�hs@�O�@�&�@���@���@���@� �@�=q@��/@�bN@�A�@���@��F@��w@�+@�n�@���@�`B@�%@�Ĝ@�z�@���@�b@��;@��D@�G�@�`B@��7@��^@��T@�M�@�|�@�ƨ@���@�+@���@��R@�ff@�V@�^5@�^5@�^5@�^5@�V@�5?@�$�@��@���@��@��-@��@�`B@�X@�X@�G�@�/@���@��`@��j@�1@�o@��\@�n�@�^5@�E�@�E�@�5?@��@�hs@�/@��@�V@�%@���@��`@��@��`@��@���@�z�@�Z@� �@��;@��w@���@���@��P@�t�@�\)@�33@��@���@���@��^@��h@�hs@��`@�Ĝ@��D@�Z@�9X@�P@~��@~ff@}�T@}O�@|�j@z�@n��@eN<@[�Q@UVm@M�@F@@�$@:E�@4�$@1j@*��@%}�@!��@ѷ@G@8@6z@��@	+@3�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A��/A��A��A��A��/A��/A��HA��HA��HA��HA��;A��;A��;A��/A��/A��HA��;A��;A��;A��HA��HA��HA��`A��`A��yA��A��`A��^A�oA���A���A�p�A�`BA�ZA�`BA�\)A�VA�5?A�(�A�(�A�"�A��A��A��A��A�bA�VA�JA�%A�1A�
=A�1A�A��A�ȴA�z�A�S�A�~�A���A��;A�;dA�ffA�oA�l�A�hsA��RA�bNA�l�A��
A���A�ƨA�ĜA�O�A��A��;A�7LA�5?A���A��PA���A�"�A��;A�^5A��A��/A�G�A��A��A��!A��A�\)A�z�A�oA��A�+A���A�r�A���A��A�~�A��A��#A��hA�$�A� �A��A?}A~��A~E�Az5?Aql�Am�
Al��AlI�Ak��Aj��Ai��Ah��Ag�#Af��A_��A]�A\1'A[��AZ�/AX�\AT��AR��AQ�;APȴAM��AKAIt�AH~�AHAF�yAC��A?l�A;A:�jA:z�A:(�A9�7A8��A733A6n�A5�A5�FA5x�A5"�A4�A4��A4n�A4I�A4A�A3�A2r�A1��A0�jA.^5A-�-A-/A,�HA,  A*�A)�#A(��A(v�A'�;A'�A&��A&-A%;dA$��A#�;A"Q�A!��A �\A7LA�A��A�yAA;dA�DA��AȴA��A��A7LA �A;dA��A �A	�-A5?A\)A�;AoA�mA/AĜAJA ^5@�M�@���@��@���@���@��;@�{@�`B@��u@��m@�l�@�@�-@��@�%@�r�@���@�"�@홚@���@�7@�o@�{@噚@�@���@㕁@�|�@�\)@�;d@�o@��y@�ȴ@⟾@�V@�@��u@߶F@��H@���@�@�=q@�O�@�l�@�hs@�&�@��@��@��;@�C�@��y@ҟ�@�^5@�x�@�I�@�K�@�{@�V@�o@�Ĝ@�Q�@Ǿw@�@��
@�x�@��@�S�@�^5@�J@���@�&�@��u@���@�33@���@�5?@�?}@�b@�33@�n�@��T@���@��@���@� �@���@���@�x�@�p�@�X@�Ĝ@�9X@���@��!@�M�@�hs@��@��@�bN@�ƨ@�S�@��P@�+@�E�@��@���@�/@��u@�bN@���@��P@�33@�o@���@���@��#@�`B@�&�@���@���@��@��@�A�@��m@�S�@��H@�V@���@��^@��@�O�@��@��@�I�@�b@��
@�o@��!@���@�G�@�/@��@��@�r�@�(�@�  @�ƨ@�|�@��H@��R@���@��+@�-@�x�@��@��@���@�r�@�A�@��@���@�t�@�dZ@�;d@�
=@��y@���@�@�hs@��9@��P@�"�@��@�{@��#@���@��@�hs@�O�@�&�@���@���@���@� �@�=q@��/@�bN@�A�@���@��F@��w@�+@�n�@���@�`B@�%@�Ĝ@�z�@���@�b@��;@��D@�G�@�`B@��7@��^@��T@�M�@�|�@�ƨ@���@�+@���@��R@�ff@�V@�^5@�^5@�^5@�^5@�V@�5?@�$�@��@���@��@��-@��@�`B@�X@�X@�G�@�/@���@��`@��j@�1@�o@��\@�n�@�^5@�E�@�E�@�5?@��@�hs@�/@��@�V@�%@���@��`@��@��`@��@���@�z�@�Z@� �@��;@��w@���@���@��P@�t�@�\)@�33@��@���@���@��^@��h@�hs@��`@�Ĝ@��D@�Z@�9X@�P@~��@~ff@}�T@}O�G�O�@z�@n��@eN<@[�Q@UVm@M�@F@@�$@:E�@4�$@1j@*��@%}�@!��@ѷ@G@8@6z@��@	+@3�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�LB�FB�FB�FB�LB�FB�FB�FB�FB�FB�FB�FB�FB�FB�FB�FB�FB�FB�FB�FB�FB�FB�FB�FB�FB�FB�FB�LB�qB��BB�dB�LB�LB�^B�qB�wB�}B�wB�}B��B�}B�}B�}B�wB�wB�qB�jB�jB�jB�jB�qB��B��BĜB��B�`B�B
=B�B"�B�B �B!�B%�B&�B%�B%�B(�B(�B&�B"�B�B�B�BDB��B�B�B�B�;B��BB�qB�dB�RB�-B��B��B�Bk�BVB@�B33B'�B�B\B
��B
��B
��B
�1B
p�B
YB
A�B
0!B
 �B
�B
�B
�B
\B
  B	��B	��B	��B	�hB	�JB	�%B	}�B	t�B	m�B	bNB	>wB	'�B	!�B	�B	{B		7B�B�`B�)B�BȴB�wB�FB�B��B��B��B��B�B�B�B�B� B~�B}�By�Bx�Bw�Bw�Bw�Bu�Bu�Bu�Bt�Bs�Br�Bq�Bm�Bk�BhsBdZBcTB`BB`BB[#BYBVBT�BR�BQ�BP�BN�BN�BL�BM�BM�BL�BK�BK�BL�BM�BL�BK�BK�BI�BL�BM�BO�BN�BN�BL�BL�BG�BA�BD�B=qB;dB7LB6FB8RB6FB33B1'B/B/B.B.B.B/B.B/B/B/B0!B33B5?B8RB8RB7LB6FB6FB9XB<jB>wBC�BF�BF�BF�BI�BJ�BJ�BK�BK�BL�BL�BM�BM�BN�BO�BO�BXBYBZBaHBffBgmBhsBn�Bm�Bl�Bl�Bm�Bo�Bp�Bp�Bp�Bq�Br�Bs�Bu�Bw�By�B� B�B�B�B�%B�7B�VB�hB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�!B�-B�FB�^B�^B�^B�^B�wBÖB��B��B��B��B��B�B�#B�BB�BB�`B�B�B�B�B�B��B��B	  B	B	B	%B	+B	1B	VB	bB	hB	oB	uB	uB	{B	�B	�B	�B	�B	"�B	$�B	'�B	)�B	,B	/B	2-B	49B	6FB	7LB	;dB	;dB	>wB	A�B	B�B	C�B	E�B	G�B	I�B	J�B	M�B	Q�B	\)B	]/B	]/B	]/B	_;B	dZB	ffB	ffB	iyB	l�B	m�B	o�B	q�B	r�B	s�B	t�B	v�B	w�B	z�B	~�B	�B	�B	�1B	�=B	�DB	�hB	�hB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�'B	�-B	�3B	�9B	�LB	�}B	B	ÖB	ĜB	ĜB	ŢB	ƨB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	ɺB	��B	��B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�5B	�5B	�5B	�/B	�/B	�)B	�)B	�/B	�;B	�NB	�`B	�mB	�mB	�mB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B
)B
=B
&�B
,WB
3hB
;B
?�B
FB
K^B
N�B
V9B
Y�B
]dB
cB
h�B
m)B
o�B
shB
x8B
|P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�nB�hB�hB�hB�nB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�nB��B��B��B��B�nB�nB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B� B݀B��B[B�B�B�B�B�B BB B B!B!BB�B�B�B�BeB�B��B��B�B�`B�B��B��B��B�zB�UB�B��B}KBc�BN3B8�B+eB #B�B�B
�B
�%B
��B
�pB
h�B
QYB
9�B
(fB
B
�B
�B
�B
�B	�IB	��B	�B	��B	��B	��B	~wB	vGB	mB	e�B	Z�B	6�B	 JB	%B	B	�B	�B�BݿBԉB�wB�B��B��B�xB�`B�HB�B��BztBynBynBynBxiBwcBv]BrDBq?Bp9Bp9Bp9Bn-Bn-Bn-Bm&Bl BkBjBe�Bc�B`�B\�B[�BX�BX�BS�BQ�BNqBMlBK`BJZBISBGGBGGBE<BFBBFBBE<BD6BD6BE=BFCBE=BD7BD7BB+BE>BFDBHPBGJBGJBE?BE?B@ B9�B=B5�B3�B/�B.�B0�B.�B+�B)�B'�B'�B&�B&�B&�B'�B&�B'�B'�B'�B(�B+�B-�B0�B0�B/�B.�B.�B1�B4�B6�B<B?B?B?BB1BC8BC8BD>BD>BEDBEDBFJBFJBGPBHVBHVBP�BQ�BR�BY�B^�B_�B`�BgBfBeBeBfBhBiBiBiBj Bk&Bl,Bn9BpEBrQBxvBy|By|By|B~�B��B��B��B��B�B�B�B�B�B�-B�3B�:B�FB�XB�qB�wB��B��B��B��B��B��B��B��B��B��B��B��B�
B�AB�AB�AB�XB�eB�wBӖBصBصB��B��B�B�B�B�B�.B�RB�qB��B��B��B��B	 �B	�B	�B		�B	
�B	�B	�B	�B	�B	�B	B	.B	AB	LB	 _B	"kB	$wB	'�B	*�B	,�B	.�B	/�B	3�B	3�B	6�B	9�B	:�B	<B	>B	@B	B(B	C/B	FAB	JYB	T�B	U�B	U�B	U�B	W�B	\�B	^�B	^�B	a�B	d�B	e�B	h
B	jB	kB	l"B	m(B	o5B	p:B	sLB	weB	yqB	|�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�"B	�(B	�;B	�GB	�`B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�"B	�"B	�)B	�)B	�)B	�)B	�"B	�:B	�LB	�kB	�~B	҄B	ӊB	ԐB	ՕB	ՖB	֛B	֛B	֛B	֜B	ՖB	ՖB	ԐB	ԐB	ՖB	עB	ڴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�"B	�/G�O�B	��B
�B
�B
B
$�B
+�B
3�B
8IB
>nB
C�B
G"B
N�B
Q�B
U�B
[gB
a	B
e�B
hMB
k�B
p�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.31 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144172022020411441720220204114417  AO  ARCAADJP                                                                    20200619170909    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170909  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170909  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114417  IP                  G�O�G�O�G�O�                