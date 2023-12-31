CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-11-01T15:00:58Z creation      
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
_FillValue                    �(   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �,   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �0   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �4   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �8   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �x   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20201101150058  20220204114430  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @�D'� �1   @�D��WI@5��t�j�b�r� Ĝ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Co�fCr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DX��DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� DcfDc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dj��Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� DrfDr� Ds  Ds�fDtfDt� Du  DuFfDy��D�fD�P�D��\D���D� RD�G�D���D�ָD� RD�QHD���D�ȤD�3D�C3Dڌ)D��
D��D�VfD�D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�Q�@�Q�@�Q�A(�A<(�A\(�A|(�A�{A�{A�{A�{A�{A�{A�{A�{B
=B
=B
=B
=B'
=B/
=B7
=B?
=BG
=BO
=BW
=B_
=Bg
=Bo
=Bw
=B
=B��B��B��B��B��B��RB��B��B��B��B��B��B��B��B��B��BÅBǅB˅BυBӅBׅBۅB߅B�B�B�B�B�B��B��B��CCCCC	CCCCCCCCCCCC!C#C%C'C)C+C-�)C/C1C3C5C7C9C;C=C?CACCCECGCICKCMCOCQCSCUCWCYC[C]C_CaCcCeCgCiCkCmCo��CqCsCuCwCyC{C}CC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��{C��{C��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HD p�D �Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�D	p�D	�D
p�D
�Dp�D�
Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dw
D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�D p�D �D!p�D!�D"p�D"�D#p�D#�D$p�D$�D%p�D%�D&p�D&�D'p�D'�D(p�D(�D)p�D)�D*p�D*�D+p�D+�D,p�D,�D-p�D-�D.p�D.�D/p�D/�D0p�D0�D1p�D1�D2p�D2�D3p�D3�D4p�D4�D5p�D5�D6p�D6�D7p�D7�D8p�D8�D9p�D9�D:p�D:�D;p�D;�D<p�D<�D=p�D=�D>p�D>�D?p�D?�D@p�D@�DAp�DA�DBp�DB�DCp�DC�DDp�DD�DEp�DE�DFp�DF�DGp�DG�DHp�DH�DIp�DI�DJp�DJ�DKp�DK�DLp�DL�DMp�DM�DNp�DN�DOp�DO�DPp�DP�DQp�DQ�DRp�DR�DSp�DS�DTp�DT�DUp�DU�DVp�DV�DWp�DW�DXp�DX�>DYp�DY�DZp�DZ�D[p�D[�D\p�D\�D]p�D]�D^p�D^�D_p�D_�D`p�D`�Dap�Da�Dbp�Db�
Dcp�Dc�Ddp�Dd�Dep�De�Dfp�Df�Dgp�Dg�Dhp�Dh�Dip�Di�Djp�Dj�>Dkp�Dk�Dlp�Dl�Dmp�Dm�Dnp�Dn�Dop�Do�Dpp�Dp�Dqp�Dq�
Drp�Dr�Dsw
Ds�
Dtp�Dt�Du7
Dy��D��D�IHD���D��)D��D�@ D���D��
D��D�I�D���D���D��D�;�Dڄ{D��\D�
�D�N�D��D�ۅ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A��A��HAԾwA�x�A�hsA�Q�A�G�A�7LA�+A�"�A��A��A��A�{A�VA�A���A��A��mA��`A���AӾwAӺ^AӸRAӸRAӮAӟ�Aӕ�Aӝ�Aӡ�AӶFA�5?A���AѼjA�Aδ9A� �A˕�A��Aȡ�A�+AăA�K�A�
=A���A��
A��A�$�A���A�5?A���A�`BA��A�ƨA��yA�C�A�ffA���A��A�$�A�bA���A�$�A���A��A�&�A�r�A���A�dZA��PA�A�A�r�A�jA��A�r�A�p�A��#A��TA�bNA�%A��uA�A�bA�bNA���A�S�A���A�7LA� �A�\)A���A��A��A��+A�l�A��A��-A��hA�ƨA���A��A�ZA��/A��A��A�=qA�^A}�A{�FAy��Aw�mAvI�As�;Aq;dAnȴAl�uAk��Ah��Ae
=AbbA`�A^bNA\��AZ�AX5?AQ��ANz�AK|�AIp�AG�PAF5?AE/AD��ACx�AAG�A?��A=�FA<n�A;7LA:-A8�\A7x�A6ZA4�A333A1�mA0n�A.�jA-"�A,��A+�A*�yA*�A*JA(�A(^5A'7LA&n�A%�^A$  A#�PA"ĜA!�A!G�A|�AZA��Av�AA�A��AK�AjAƨAE�Ap�A�!A�;A��A��A�yA9XA�;A;dAbNA�AK�An�A�A
�HA
�jA	��A�/AƨA��AffAAA�/A�-A�wAƨA��A�A ��A z�@��w@��@��@�I�@���@�~�@��h@�bN@��@�ȴ@��@�p�@�j@�F@�K�@�+@�@�@�Q�@�+@�=q@�hs@�b@��H@��@�9@�o@�J@�9@�J@��D@��@�l�@݉7@ܬ@�33@١�@�Q�@�"�@Չ7@�(�@���@�\)@щ7@�"�@�E�@���@��@ɉ7@Ǯ@��@�O�@��`@�7L@��@���@Ĭ@��@Õ�@��@��@�`B@�%@��;@��R@��\@��7@���@�ȴ@��+@�5?@�%@��;@�C�@�33@�
=@�J@�x�@�%@��j@�I�@���@���@��@�;d@��+@��@���@���@���@�C�@���@�^5@�$�@���@���@��@�G�@��@���@��;@�+@���@��+@�ff@��#@��/@�Z@��F@���@�l�@�K�@�@��y@�
=@��y@��+@�$�@��@�-@�v�@�~�@�J@���@��-@��@���@�Q�@�ȴ@���@�-@�O�@��@�I�@��w@�\)@�C�@�K�@�
=@���@���@�~�@�-@�=q@�5?@�{@��T@�hs@�`B@��@�j@�Q�@�1'@�(�@��@���@�?}@�V@��@���@���@�hs@���@���@��#@�J@���@�z�@�bN@�(�@�  @��@�o@�ȴ@���@�5?@��-@�hs@�G�@�/@���@�X@���@���@��`@��j@�Z@�  @���@��@��H@��y@���@���@�V@�@���@��@�v�@�@���@�%@��`@��/@��9@�j@�I�@�bN@��@�I�@��
@��m@�+@�v�@�5?@��@���@�O�@�?}@�%@���@�j@�1@�ƨ@��F@���@���@�dZ@�S�@��@�ȴ@��!@�^5@�$�@��@��T@��@�J@��^@��@�X@�&�@��@���@��@�r�@�Q�@�9X@�(�@���@�S�@��@���@�M�@��@���@�p�@�?}@�&�@��@�V@���@��@�Ĝ@�z�@�9X@��;@��P@�;d@�o@��y@���@���@���@�ff@�5?@�{@���@���@��@y�@q�@i`B@a��@Zl�@P�@I�.@D�@>a|@7&@1j@,A�@'�
@$b@ �?@�@�"@1�@n�@
�+1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A���A���A���A���A���A��A��HAԾwA�x�A�hsA�Q�A�G�A�7LA�+A�"�A��A��A��A�{A�VA�A���A��A��mA��`A���AӾwAӺ^AӸRAӸRAӮAӟ�Aӕ�Aӝ�Aӡ�AӶFA�5?A���AѼjA�Aδ9A� �A˕�A��Aȡ�A�+AăA�K�A�
=A���A��
A��A�$�A���A�5?A���A�`BA��A�ƨA��yA�C�A�ffA���A��A�$�A�bA���A�$�A���A��A�&�A�r�A���A�dZA��PA�A�A�r�A�jA��A�r�A�p�A��#A��TA�bNA�%A��uA�A�bA�bNA���A�S�A���A�7LA� �A�\)A���A��A��A��+A�l�A��A��-A��hA�ƨA���A��A�ZA��/A��A��A�=qA�^A}�A{�FAy��Aw�mAvI�As�;Aq;dAnȴAl�uAk��Ah��Ae
=AbbA`�A^bNA\��AZ�AX5?AQ��ANz�AK|�AIp�AG�PAF5?AE/AD��ACx�AAG�A?��A=�FA<n�A;7LA:-A8�\A7x�A6ZA4�A333A1�mA0n�A.�jA-"�A,��A+�A*�yA*�A*JA(�A(^5A'7LA&n�A%�^A$  A#�PA"ĜA!�A!G�A|�AZA��Av�AA�A��AK�AjAƨAE�Ap�A�!A�;A��A��A�yA9XA�;A;dAbNA�AK�An�A�A
�HA
�jA	��A�/AƨA��AffAAA�/A�-A�wAƨA��A�A ��A z�@��w@��@��@�I�@���@�~�@��h@�bN@��@�ȴ@��@�p�@�j@�F@�K�@�+@�@�@�Q�@�+@�=q@�hs@�b@��H@��@�9@�o@�J@�9@�J@��D@��@�l�@݉7@ܬ@�33@١�@�Q�@�"�@Չ7@�(�@���@�\)@щ7@�"�@�E�@���@��@ɉ7@Ǯ@��@�O�@��`@�7L@��@���@Ĭ@��@Õ�@��@��@�`B@�%@��;@��R@��\@��7@���@�ȴ@��+@�5?@�%@��;@�C�@�33@�
=@�J@�x�@�%@��j@�I�@���@���@��@�;d@��+@��@���@���@���@�C�@���@�^5@�$�@���@���@��@�G�@��@���@��;@�+@���@��+@�ff@��#@��/@�Z@��F@���@�l�@�K�@�@��y@�
=@��y@��+@�$�@��@�-@�v�@�~�@�J@���@��-@��@���@�Q�@�ȴ@���@�-@�O�@��@�I�@��w@�\)@�C�@�K�@�
=@���@���@�~�@�-@�=q@�5?@�{@��T@�hs@�`B@��@�j@�Q�@�1'@�(�@��@���@�?}@�V@��@���@���@�hs@���@���@��#@�J@���@�z�@�bN@�(�@�  @��@�o@�ȴ@���@�5?@��-@�hs@�G�@�/@���@�X@���@���@��`@��j@�Z@�  @���@��@��H@��y@���@���@�V@�@���@��@�v�@�@���@�%@��`@��/@��9@�j@�I�@�bN@��@�I�@��
@��m@�+@�v�@�5?@��@���@�O�@�?}@�%@���@�j@�1@�ƨ@��F@���@���@�dZ@�S�@��@�ȴ@��!@�^5@�$�@��@��T@��@�J@��^@��@�X@�&�@��@���@��@�r�@�Q�@�9X@�(�@���@�S�@��@���@�M�@��@���@�p�@�?}@�&�@��@�V@���@��@�Ĝ@�z�@�9X@��;@��P@�;d@�o@��y@���@���@���@�ff@�5?@�{@���G�O�@��@y�@q�@i`B@a��@Zl�@P�@I�.@D�@>a|@7&@1j@,A�@'�
@$b@ �?@�@�"@1�@n�@
�+1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B �B �B"�B$�B&�B(�B)�B,B-B.B/B1'B49B8RB9XB;dB<jBA�BM�BQ�BW
B`BBgmBl�Bk�Bv�B�7B�LB��B��B%B�B�B�B�BhB	7B��B�`B�HB�NB�HB�/B�fB�yB�fB�`B�B��B  B{B�B�B#�B,B2-B9XBH�BH�BQ�BYB\)B`BB]/BXBT�BR�BVBT�BL�BF�B<jB2-B/B(�B#�B�B+B�B�fB�5B�BǮB�qB��B��B�=Bs�B]/BB�B1'B �BB
�B
�B
�BB
ŢB
��B
��B
z�B
e`B
[#B
G�B
0!B
 �B
�B
B	�B	�`B	�B	ÖB	�-B	��B	��B	�B	n�B	T�B	G�B	<jB	-B	!�B	hB�B�)B��BB�^B�3B�B�B��B��B��B�\B�DB�1B�B�1B�B�B~�B}�B{�Bw�Br�Bs�Bu�Bu�Br�Br�Br�Br�Bq�Bq�Bn�Bm�BjBhsBiyBgmBffBbNBffBe`BcTB`BBffBk�Bk�Bl�Bl�BiyBhsBiyBffBe`BaHBbNBbNBcTBffBffBdZBffBdZBdZBcTBbNBcTBaHB`BB`BBdZBe`BgmBffBcTBffBn�Bq�Bq�Bs�Bt�Bw�Bx�Bw�Bx�B{�B}�B}�B|�B� B�B� B�B�B�B�B�B�B�B�B�B�7B�PB�VB�VB�\B�VB�=B�7B�1B�B~�B�B~�By�Bx�B|�B{�B�B� B� B� B~�B~�B~�By�Bv�Bt�Bq�Br�Bt�Bx�By�B�B�JB�hB�hB�hB�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�FB�LB�FB�LB�XB�jB��B��BÖBŢBǮBɺB��B��B��B��B��B��B��B��B�
B�)B�)B�/B�/B�BB�TB�`B�yB�B�B�B�B�B�B��B��B��B	B	1B	bB	uB	�B	{B	�B	�B	�B	�B	�B	�B	#�B	#�B	+B	-B	-B	1'B	2-B	6FB	9XB	9XB	:^B	=qB	>wB	C�B	G�B	H�B	I�B	M�B	O�B	Q�B	Q�B	R�B	S�B	VB	]/B	dZB	k�B	n�B	l�B	q�B	x�B	{�B	�B	�B	�=B	�VB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�9B	�?B	�LB	�RB	�XB	�XB	�XB	�^B	�jB	�^B	�LB	�3B	�'B	�'B	�?B	�LB	�dB	�jB	�jB	�wB	�}B	ĜB	ȴB	ȴB	��B	ɺB	ȴB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�#B	�)B	�/B	�5B	�BB	�BB	�BB	�NB	�ZB	�`B	�fB	�mB	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
'B
HB
B
$�B
,WB
2�B
="B
CB
HKB
N�B
T{B
YKB
^�B
cB
f�B
i�B
p�B
s�B
w2B
{�B
�i1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B
�B
�B	zB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!B$B(/B)5B+@B,FB1eB=�BA�BF�BPBWDB\aB[\Bf�By
B�B�B�B��BJBiB�B{B-B��B�B�,B�B�B�B��B�5B�HB�6B�1B�B�B��BHBYB~B�B�B!�B)"B8|B8|BA�BH�BK�BPBL�BG�BD�BB�BE�BD�B<�B6sB,7B!�B�B�B�B`B� B�qB�@B�B��B��B�QB��B�mBz%Bc�BMB2�B!B�B
�B
ߜB
�~B
�BB
��B
��B
��B
j�B
UqB
K6B
7�B
 :B
�B
�B	�,B	��B	ՄB	�=B	��B	�YB	��B	��B	uRB	^�B	E8B	7�B	,�B	NB	B	�B��B�uB�B��B��B��B�oB�VB�2B�	B��B�B{�Bx�BuzBx�BtuBriBoWBnRBlEBh.BcBdBf#Bf$BcBcBcBcBbBbB^�B]�BZ�BX�BY�BW�BV�BR�BV�BU�BS�BP�BV�B[�B[�B\�B\�BY�BX�BY�BV�BU�BQ�BR�BR�BS�BV�BV�BT�BV�BT�BT�BS�BR�BS�BQ�BP�BP�BT�BU�BW�BV�BS�BV�B_BbBbBd!Be'Bh:Bi@Bh:Bi@BlRBn_Bn_BmYBpkBs}BpkBs}Bs}BrwBrwBs}Bu�Bs}Bs}BrxBy�B}�B~�B~�B�B~�Bz�By�Bx�Bu�BohBqtBohBjJBiDBm]BlVBquBpoBpoBpoBojBojBojBjLBg:Be.BbBc"Be.BiGBjMBs�B|�B��B��B��B��B��B��B��B��B�B�	B�B�!B�!B�B�B�B�!B�"B�:B�LB�XB�dB�XB�_B��B��B��B��B��B��B��B��B��B��B�B�B�B�&B�-B�-B�3B�8B�>B�JB�PB�WB�tB̓B̓B͙B͙BЬBӽB��B��B��B��B��B� B� B�B�6B�HB�UB�xB��B	 �B	�B	�B	�B	�B		�B	B	B	B	B	:B	:B	dB	pB	pB	!�B	"�B	&�B	)�B	)�B	*�B	-�B	.�B	3�B	8B	9B	:B	>1B	@=B	BJB	BJB	CPB	DVB	FaB	M�B	T�B	[�B	^�B	\�B	bB	i.B	l@B	q^B	sjB	z�B	~�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	� B	�B	�B	�B	�+B	�DB	�VB	�VB	�gB	�nB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	� B	� B	�B	�B	�B	�$B	�*B	�0B	�6B	�6B	�6B	�6B	�=B	�CB	�CB	�TB	�TB	�ZB	�gB	�mB	�sB	�yB	�B	΅B	БB	БB	БB	ҝB	ԩB	կB	ֵB	׼B	׼B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�"B	�"B	�"B	�"B	�.B	�4B	�;B	�;G�O�B	�tB
 �B
]B
�B
�B
"�B
-jB
3ZB
8�B
? B
D�B
I�B
N�B
SKB
V�B
Z(B
aB
c�B
gwB
lB
p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.24 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.015(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144302022020411443020220204114430  AO  ARCAADJP                                                                    20201101150058    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20201101150058    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20201101150058  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20201101150058  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114430  IP                  G�O�G�O�G�O�                