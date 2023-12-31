CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-18T14:14:02Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200618141402  20220204114420  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               aA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @��^)V�1   @��^���@6pbM���c�|�hs1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    aA   B   B   @�ff@�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB��B   B'33B0��B8  B@  BH  BO��BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D0��D1y�D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7fD7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP�fDQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy��D�%D�X�D���D�ڏD�%D�T)D���D��qD��D�W
D��RD���D� �D�O\Dڐ D��)D�(RD�d{D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @r�[@�{@��HAp�A9p�AYp�Ayp�A��RA��RA��RA��RA̸RAܸRA�RA��RB\)BB(�B\)B%�\B/(�B6\)B>\)BF\)BM��BV\)B^Bf\)Bn\)Bv\)B~\)B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B���B�.B�.B�.B�.B�.B�.B�.C�
C�
C�
C�
C	�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C!�
C#�
C%�
C'�
C)�
C+�
C-�
C/�
C1�
C3�
C5�
C7�
C9�
C;�
C=�
C?�
CA�
CC�
CE�
CG�
CI�
CK�
CM�
CO�
CQ�
CS�
CU�
CW�
CY�
C[�
C]�
C_�
Ca�
Cc�
Ce�
Cg�
Ci�
Ck�
Cm�
Co�
Cq�
Cs�
Cu�
Cw�
Cy�
C{�
C}�
C�
C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅D e�D ��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��D	e�D	��D
e�D
��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��D e�D ��D!e�D!��D"e�D"��D#e�D#��D$e�D$��D%e�D%��D&e�D&��D'e�D'��D(e�D(��D)e�D)��D*e�D*��D+e�D+��D,e�D,��D-e�D-��D.e�D.��D/e�D/��D0e�D0�]D1_]D1��D2e�D2��D3e�D3��D4e�D4��D5e�D5��D6e�D6�)D7e�D7��D8e�D8��D9e�D9��D:e�D:��D;e�D;��D<e�D<��D=e�D=��D>e�D>��D?e�D?��D@e�D@��DAe�DA��DBe�DB��DCe�DC��DDe�DD��DEe�DE��DFe�DF��DGe�DG��DHe�DH��DIe�DI��DJe�DJ��DKe�DK��DLe�DL��DMe�DM��DNe�DN��DOe�DO��DPl)DP��DQe�DQ��DRe�DR��DSe�DS��DTe�DT��DUe�DU��DVe�DV��DWe�DW��DXe�DX��DYe�DY��DZe�DZ��D[e�D[��D\e�D\��D]e�D]��D^e�D^��D_e�D_��D`e�D`��Dae�Da��Dbe�Db��Dce�Dc��Dde�Dd��Dee�De��Dfe�Df��Dge�Dg��Dhe�Dh��Die�Di��Dje�Dj��Dke�Dk��Dle�Dl��Dme�Dm��Dne�Dn��Doe�Do��Dpe�Dp��Dqe�Dq��Dre�Dr��Dse�Ds��Dte�Dt�]Dy�qD� D�K�D���D��pD� D�G
D���D��RD��D�I�D�{3D���D��D�B=Dڂ�D�
D�3D�W\D��D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A��A�JA�
=A�{A�"�A�(�A�&�A�&�A�&�A�$�A�(�A�(�A�&�A�$�A�"�A� �A�oA�7LA�-A��mA͉7A˙�A�r�Aŝ�A�VA�A��hA�7LA�\)A�I�A�^5A���A�bA�r�A��/A�A�A�ffA��#A���A���A��A�S�A��yA�ffA���A�ZA�XA�^5A�  A�p�A��A�%A��-A��^A���A�l�A���A�
=A�\)A�hsA�n�A�{A��9A��A��yA�&�A���A�jA���A�I�A��;A�bA��-A�~�A�33A��A��RA�ZA�33A��-A�1'A�|�A���A�E�A�x�A�K�A���A��A�dZA���A�=qA�9XA�S�A~�A}?}Ay��Av��AvbNAuƨAt{Ar�RAq�hAo��Am�7Akx�Ah=qAf �Ad��Ac�AcXAb�RA`�/A_�A^=qA]`BA\��A[dZAZQ�AY7LAW�AVA�AT1'AS?}AQ�AQS�AM|�AK��AIAG��AE\)ADZACdZAA&�A@I�A?\)A>�A>A�A=�PA<�+A;��A;C�A:��A:1A8��A7x�A6�/A6jA5`BA4=qA3|�A2��A1x�A1
=A0��A/|�A.��A.-A-A-�7A,�A,�uA,^5A,1A+��A+�A)ƨA(JA'XA'A&��A&�A$��A#|�A#G�A#;dA"��A!`BA!33A r�AdZA�DA��AhsA"�A�`A��A�PA�HAl�AQ�A�PAE�AG�A�\AbAK�A1A33A^5A;dAXA�A
  A	33A�`A�yAv�A�FAI�A�FA+A��AffA�^A �A -@��m@�;d@�p�@���@��@�ff@���@�V@��u@��
@���@�%@�M�@�1'@�;d@�-@�x�@�I�@�F@�l�@���@�&�@�u@�l�@�$�@�^@�%@�dZ@�/@��@��`@�Q�@�
=@��@�1'@�-@�p�@��
@θR@�7L@̋D@̋D@̋D@̋D@�S�@�dZ@��@Ĵ9@�ȴ@��h@�r�@���@���@���@�1@�
=@�?}@�ƨ@��@��@��@�bN@���@�^5@��@�7L@��9@��w@��@��@�~�@��@�hs@��9@�|�@��H@��R@�~�@���@�p�@�V@��/@��/@���@���@�(�@�C�@���@�M�@�5?@��@�@��@��D@�r�@�bN@���@�o@��+@��+@�~�@�5?@��T@���@��h@�X@�9X@��@�
=@�E�@�7L@��u@�1'@�b@��m@��F@�l�@�S�@�"�@���@�ȴ@�^5@�{@�`B@��@�Ĝ@��u@�z�@�Z@�I�@�1'@�b@�  @��w@�33@��@��y@��y@���@���@��\@�J@��@�`B@�7L@��@���@���@��/@��D@�A�@�9X@�1'@�1@���@���@��@�S�@��@��H@���@��+@�v�@���@��R@��+@�n�@�V@�$�@�@�7L@��`@��/@��@�1@�+@�S�@�;d@��@���@�n�@�^5@�5?@�-@�-@�n�@���@�=q@�@��@��7@���@�ff@���@��H@�o@�K�@�33@�"�@�@�
=@��@���@�=q@��#@�@��7@�V@�Ĝ@��u@�z�@�bN@�9X@�b@���@���@�dZ@�K�@�33@�+@��@��y@��!@��\@�V@��@���@��#@�hs@�7L@���@���@���@�1@��F@��@��@���@��@�S�@��@�
=@�@���@��@��!@�~�@�{@��#@�@���@���@���@�O�@��@��@��@��/@�z�@��@���@��w@���@�K�@��y@���@���@���@��H@��@�	@y-w@j��@e��@[@T(�@Mԕ@F�b@=�'@6ȴ@/E9@+n/@&4@"$�@�P@1@Ov@��@��@	k�@�8111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��A��A��A��A�JA�
=A�{A�"�A�(�A�&�A�&�A�&�A�$�A�(�A�(�A�&�A�$�A�"�A� �A�oA�7LA�-A��mA͉7A˙�A�r�Aŝ�A�VA�A��hA�7LA�\)A�I�A�^5A���A�bA�r�A��/A�A�A�ffA��#A���A���A��A�S�A��yA�ffA���A�ZA�XA�^5A�  A�p�A��A�%A��-A��^A���A�l�A���A�
=A�\)A�hsA�n�A�{A��9A��A��yA�&�A���A�jA���A�I�A��;A�bA��-A�~�A�33A��A��RA�ZA�33A��-A�1'A�|�A���A�E�A�x�A�K�A���A��A�dZA���A�=qA�9XA�S�A~�A}?}Ay��Av��AvbNAuƨAt{Ar�RAq�hAo��Am�7Akx�Ah=qAf �Ad��Ac�AcXAb�RA`�/A_�A^=qA]`BA\��A[dZAZQ�AY7LAW�AVA�AT1'AS?}AQ�AQS�AM|�AK��AIAG��AE\)ADZACdZAA&�A@I�A?\)A>�A>A�A=�PA<�+A;��A;C�A:��A:1A8��A7x�A6�/A6jA5`BA4=qA3|�A2��A1x�A1
=A0��A/|�A.��A.-A-A-�7A,�A,�uA,^5A,1A+��A+�A)ƨA(JA'XA'A&��A&�A$��A#|�A#G�A#;dA"��A!`BA!33A r�AdZA�DA��AhsA"�A�`A��A�PA�HAl�AQ�A�PAE�AG�A�\AbAK�A1A33A^5A;dAXA�A
  A	33A�`A�yAv�A�FAI�A�FA+A��AffA�^A �A -@��m@�;d@�p�@���@��@�ff@���@�V@��u@��
@���@�%@�M�@�1'@�;d@�-@�x�@�I�@�F@�l�@���@�&�@�u@�l�@�$�@�^@�%@�dZ@�/@��@��`@�Q�@�
=@��@�1'@�-@�p�@��
@θR@�7L@̋D@̋D@̋D@̋D@�S�@�dZ@��@Ĵ9@�ȴ@��h@�r�@���@���@���@�1@�
=@�?}@�ƨ@��@��@��@�bN@���@�^5@��@�7L@��9@��w@��@��@�~�@��@�hs@��9@�|�@��H@��R@�~�@���@�p�@�V@��/@��/@���@���@�(�@�C�@���@�M�@�5?@��@�@��@��D@�r�@�bN@���@�o@��+@��+@�~�@�5?@��T@���@��h@�X@�9X@��@�
=@�E�@�7L@��u@�1'@�b@��m@��F@�l�@�S�@�"�@���@�ȴ@�^5@�{@�`B@��@�Ĝ@��u@�z�@�Z@�I�@�1'@�b@�  @��w@�33@��@��y@��y@���@���@��\@�J@��@�`B@�7L@��@���@���@��/@��D@�A�@�9X@�1'@�1@���@���@��@�S�@��@��H@���@��+@�v�@���@��R@��+@�n�@�V@�$�@�@�7L@��`@��/@��@�1@�+@�S�@�;d@��@���@�n�@�^5@�5?@�-@�-@�n�@���@�=q@�@��@��7@���@�ff@���@��H@�o@�K�@�33@�"�@�@�
=@��@���@�=q@��#@�@��7@�V@�Ĝ@��u@�z�@�bN@�9X@�b@���@���@�dZ@�K�@�33@�+@��@��y@��!@��\@�V@��@���@��#@�hs@�7L@���@���@���@�1@��F@��@��@���@��@�S�@��@�
=@�@���@��@��!@�~�@�{@��#@�@���@���@���@�O�@��@��@��@��/@�z�@��@���@��w@���@�K�@��y@���@���@���@��HG�O�@�	@y-w@j��@e��@[@T(�@Mԕ@F�b@=�'@6ȴ@/E9@+n/@&4@"$�@�P@1@Ov@��@��@	k�@�8111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBcTBbNBcTBcTBcTBcTBcTBcTBbNBbNBbNBcTBcTBcTBcTBbNBbNBbNBcTB`BBffB��B�BBuBH�Bl�B�+B�XB�}B�}B��B��BǮB��BɺBǮBǮBƨBĜB�^B�-B�B��B�{B�JB�BcTB8RB �BVB
=B�B�mBJB�B#�BT�BQ�BS�BS�BJ�B@�B2-B.B'�B�BhBB��B�B�`B�;B��B�dB�3B�B��B��B��B�VBv�Bk�BaHBP�B7LB�B
=B
��B
�fB
�B
��B
�!B
��B
�B
jB
VB
I�B
1'B
uB
\B
JB
B	��B	�B	�;B	��B	�wB	��B	�\B	�1B	�B	z�B	u�B	m�B	_;B	W
B	R�B	J�B	E�B	<jB	6FB	,B	$�B	�B	oB	JB	%B��B�B�`B�HB�/B�B��BɺBƨBÖBŢBĜB�}B�XB�LB�9B�'B�B��B��B��B��B��B��B��B��B�uB�{B��B��B�oB�hB�oB��B�oB�oB�bB�VB�DB�1B�B|�Bw�Bv�Bv�Bs�Bt�Bp�Bo�Bn�Bp�Bk�BjBn�Bt�Bp�Bm�Bm�Bl�Bk�Bk�BjBffB`BB[#BXBS�BP�BO�BM�BM�BK�BI�BH�BH�BC�B?}B5?B49B2-B49B7LB;dB;dB8RB7LB5?B2-B/B2-B33B1'B2-B33B2-B2-B1'B1'B0!B/B-B-B+B%�B,B+B,B-B0!B33B9XB@�B=qB9XB;dB>wB>wBA�BD�BH�BK�BN�BM�BO�BM�BM�BN�BM�BO�BP�BW
BXBW
BW
BVBW
BO�BO�BP�BQ�BS�BVBVB]/B^5B`BBcTBgmBiyBjBo�Bq�Bq�Bs�Bw�By�B{�B{�B}�B~�B}�B~�B� B�B�B�%B�7B�=B�JB�bB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�^B�wB��B��BĜBƨBƨBȴB��B��B�
B�B�5B�mB�B�B�B�B��B��B��B��B��B��B��B	B		7B	DB	\B	bB	oB	uB	{B	�B	�B	�B	�B	 �B	"�B	"�B	"�B	$�B	&�B	&�B	,B	0!B	5?B	8RB	8RB	9XB	:^B	;dB	<jB	?}B	@�B	B�B	E�B	H�B	J�B	L�B	N�B	R�B	T�B	VB	YB	\)B	`BB	ffB	iyB	jB	jB	m�B	q�B	s�B	t�B	t�B	t�B	r�B	s�B	z�B	{�B	� B	�B	�B	�B	�B	�B	�%B	�DB	�DB	�PB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�!B	�!B	�'B	�3B	�FB	�RB	�XB	�XB	�^B	�dB	�jB	�}B	��B	��B	��B	B	ÖB	ĜB	ĜB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�5B	�;B	�;B	�;B	�HB	�HB	�NB	�`B	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
�B
�B
�B
]B
"�B
-�B
6zB
=�B
DMB
KxB
S[B
Y�B
^B
ezB
jKB
m�B
pUB
u�B
zDB
~(B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BZXBYRBZXBZXBZXBZXBZXBZXBYRBYRBYRBZXBZXBZXBZXBYRBYRBYRBZXBWFB]kB��B�B�B
qB?�Bc�B~!B�KB�pB�qB�}B��B��B¼B��B��B��B��B��B�VB�%B�B��B�vB�FBxBZSB/UB�B\BDB�B�wBQB�B�BLBH�BJ�BJ�BA�B7�B)3B%B�B�BqB�B��B�B�mB�IB��B�uB�DB�&B�B��B��B�jBm�Bb�BX`BG�B.hB�B\B
��B
݈B
�:B
��B
�HB
��B
y7B
a�B
M2B
@�B
(YB

�B
�B
B	�<B	��B	��B	�tB	�&B	��B	�!B	��B	qB	xGB	r"B	mB	d�B	VB	NOB	J7B	BB	<�B	3�B	-�B	#QB	&B	�B		�B	�B�qB�B��BܰBؘBԀB�VB�JB�B��B��B��B��B��B��B��B��B�}B�qB�RB�:B�4B�.B�.B�
B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B�BznBtJBo,Bn&Bn&BkBlBhBf�Be�BhBb�Ba�Be�BlBhBd�Bd�Bc�Bb�Bb�Ba�B]�BW�BR�BOrBKZBHHBGBBE6BE6BC+BAB@B@B:�B6�B,�B+�B)�B+�B.�B2�B2�B/�B.�B,�B)�B&�B)�B*�B(�B)�B*�B)�B)�B(�B(�B'�B&�B$xB$xB"mBNB#sB"mB#sB$yB'�B*�B0�B7�B4�B0�B2�B5�B5�B8�B<B@BC2BFDBE>BGJBE>BE>BFDBE>BGJBHQBNuBO{BNuBNuBMoBNuBGKBGKBHRBIYBKeBMqBMqBT�BU�BW�BZ�B^�B`�Ba�Bg
BiBiBk"Bo;BqGBsSBsSBu`BvfBu`BvfBwlByxBz~B}�B��B��B��B��B��B��B��B��B�B�B�B�(B�"B�"B�"B�"B�B�;B�fB�lB�rB��B��B��B��B��B�B�B�B�B�/B�_B�qBфB՜B��B��B��B�
B�B�"B�/B�5B�;B�FB�LB�_B�qB	 �B	�B	�B	�B		�B	
�B	�B	�B	�B	�B	B	(B	4B	4B	4B	@B	LB	LB	#kB	'�B	,�B	/�B	/�B	0�B	1�B	2�B	3�B	6�B	7�B	9�B	=B	@B	B"B	D.B	F9B	JRB	L^B	MdB	PwB	S�B	W�B	]�B	`�B	a�B	a�B	d�B	iB	kB	lB	lB	lB	jB	kB	r?B	sEB	w^B	yiB	zoB	{vB	{vB	||B	}�B	��B	��B	��B	��B	��B	��B	��B	�&B	�2B	�DB	�KB	�WB	�hB	�uB	�uB	�uB	�{B	��B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�&B	�2B	�8B	�>B	�>B	�DB	�VB	�bB	�bB	�bB	�bB	�hB	�hB	�oB	�uB	�{B	�{B	ӁB	ӁB	ՍB	֓B	֓B	֓B	ؠB	ؠB	٦B	ܷB	ݽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�+G�O�B	��B
�B
�B
�B
�B
%OB
-�B
5B
;�B
B�B
J�B
Q;B
UTB
\�B
a�B
eLB
g�B
l�B
q�B
uyB
wk111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.41 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.005) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144202022020411442020220204114420  AO  ARCAADJP                                                                    20200618141402    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200618141402  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200618141402  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114420  IP                  G�O�G�O�G�O�                