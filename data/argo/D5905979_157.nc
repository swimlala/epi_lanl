CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:31Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170931  20220204114427  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @�"\�;�q1   @�"]I���@5�V��b�-1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5y�D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DL��DM� DN  DN� DN��DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW�fDX  DX� DY  DY� DZfDZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DtfDt� Dt��Dy��D�#�D�a�D���D��\D�qD�K�D���D�ϮD�(RD�R=D���D���D�D�P�Dڗ�D�ϮD�\D�VD��D��q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�Q�@�Q�A(�A:�\A\(�A|(�A�{A�{A�{A�{A�{A�{A�{A�{B
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
=B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BÅBǅB˅BυBӅBׅBۅB߅B�B�B�B�B�B��B��B��CCCCC	CC�)C�)CCCCCCCCC!C#C%C'C)C+C-C/C1C3C5C7C9C;C=C?CACCCECGCICK��CMCOCQCSCUCWCYC[C]C_CaCcCeCgCiCkCmCoCqCsCuCwCyC{C}CC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��{C��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��C��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��{C��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HD p�D �Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�D	p�D	�D
p�D
�Dp�D�>Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dw
D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�D p�D �D!p�D!�D"p�D"�D#p�D#�D$p�D$�D%p�D%�D&p�D&�D'p�D'�D(p�D(�D)p�D)�D*p�D*�D+p�D+�D,p�D,�D-p�D-�D.p�D.�D/p�D/�D0p�D0�D1p�D1�D2p�D2�D3p�D3�D4p�D4�D5j>D5�D6p�D6�D7p�D7�D8p�D8�D9p�D9�D:p�D:�D;p�D;�D<p�D<�D=p�D=�D>p�D>�D?p�D?�D@p�D@�DAp�DA�DBp�DB�DCp�DC�DDp�DD�DEp�DE�DFp�DF�DGp�DG�DHp�DH�DIp�DI�DJp�DJ�DKp�DK�DLp�DL�>DMp�DM�DNp�DN�>DOp�DO�DPp�DP�DQp�DQ�DRp�DR�DSp�DS�DTp�DT�DUp�DU�DVp�DV�DWw
DW�DXp�DX�DYp�DY�
DZp�DZ�D[p�D[�D\p�D\�D]p�D]�D^p�D^�D_p�D_�D`p�D`�Dap�Da�Dbp�Db�Dcp�Dc�Ddp�Dd�Dep�De�Dfp�Df�Dgp�Dg�Dhp�Dh�Dip�Di�Djp�Dj�Dkp�Dk�Dlp�Dl�Dmp�Dm�Dnp�Dn�Dop�Do�Dpp�Dp�Dqp�Dq�Drp�Dr�Dsp�Ds�
Dtp�Dt�qDy�\D��D�Z>D�� D��D��D�C�D���D�� D� �D�J�D��D��D�qD�IHDڐ D�� D��D�NfD�3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A�A�A�A�  A���A���A�  A�  A�A�1A�1A�1A�1A�
=A�
=A�JA��A��A��yAȾwAȥ�Aȩ�AȸRAȲ-AȃAǶFAŕ�A�A�z�A�O�A�A��A�O�A���A�33A���A��\A�bNA�{A��A�S�A���A�oA���A��A���A���A���A�z�A�dZA�x�A�=qA�ĜA���A���A�C�A�?}A���A��\A��yA�`BA�;dA�/A��yA�$�A�G�A�I�A�C�A�$�A��A�JA���A��`A��A��+A��9A�bA�XA�M�A���A�ffA��A�ȴA�ZA�r�A���A��A�^5A�\)A���A��A���A���A�Q�A��A�n�A�%A�p�A�M�A���A���A�"�A��A��jA���A��mA���A�5?A��yA�I�A�ZA�;dA��9A�z�A�{A��!A���A��A~E�A{�
Ax�yAv��At��AqK�AmoAj�jAhĜAeƨAc
=A`=qA^-A\��A\M�AY��AV~�AT9XAR1AO�wAO&�AN��AM�AL�+AK�hAH^5AG��AE��AD5?AB�jAAhsA?��A>��A=XA;�#A;p�A:ZA9XA8��A61'A57LA4�uA3%A2^5A1�#A0�uA/"�A.��A.�A-\)A+l�A((�A&9XA$��A#p�A!�mA�mA+A �AhsA^5A1AXA$�At�AVA��Ax�Av�AbAx�AVA��AdZA��AVAƨA��A�;AK�AJAZAdZA
��A	t�A��A��A	|�AS�A��AjAK�A��AQ�A%@�ƨ@���@��y@�
=@��!@��@��^@��/@�Z@�`B@�ff@�\@�^5@���@�33@���@��^@�Q�@��@�Z@�
=@��H@���@�Z@���@ޏ\@��
@ڗ�@��@ؼj@�1@�o@�E�@ղ-@��/@�Z@��y@�"�@���@�
=@�^5@·+@͡�@�Ĝ@��@��@�A�@��@�bN@��H@��@�~�@ř�@ċD@�\)@¸R@�-@�`B@�9X@��F@�dZ@��!@�E�@��T@���@��D@�S�@�5?@��7@�7L@���@�A�@��
@�l�@��^@��@�33@���@��H@��@���@�@��-@�V@�9X@���@�|�@��@��!@�-@��@��F@���@��@��T@���@�G�@�%@�z�@�
=@���@�ȴ@���@�J@��-@�X@�O�@��@�Z@� �@�1@��;@��@��P@�C�@�"�@�n�@�-@��@��^@���@��@�G�@��@��/@��D@��
@�|�@�o@��R@���@�n�@�E�@�$�@�J@�@���@��-@���@��@�`B@���@��9@��u@��D@�z�@�(�@���@�"�@�o@���@���@���@��+@�V@�{@��T@�@��h@�X@�/@���@��@�z�@�9X@���@��@�|�@�K�@�"�@��@��\@�$�@�@�7L@�%@���@��@�  @��@�ƨ@�ƨ@��@�\)@���@�^5@�{@���@�p�@�7L@��@���@���@�r�@� �@�ƨ@��@�+@��@�"�@�K�@��H@��!@���@��\@�v�@�@��#@��-@�X@��@��/@���@��D@�Z@�Z@��@��@�33@�33@���@��@�ȴ@�~�@�M�@��@���@��-@�/@�Q�@��w@���@�l�@�t�@���@��F@���@��
@��m@�ƨ@�|�@��H@�v�@�5?@��#@���@�`B@�/@��@��/@��j@���@��@��@�V@�Ĝ@��u@�r�@�Z@�9X@���@��w@��@���@���@���@�dZ@�dZ@�l�@��y@�V@���@y�@r@�@k�@`�v@Z�x@S��@K��@H�U@C�a@;Z�@5��@01'@,4n@(��@"v�@�5@!-@��@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A���A���A���A���A�A�A�A�  A���A���A�  A�  A�A�1A�1A�1A�1A�
=A�
=A�JA��A��A��yAȾwAȥ�Aȩ�AȸRAȲ-AȃAǶFAŕ�A�A�z�A�O�A�A��A�O�A���A�33A���A��\A�bNA�{A��A�S�A���A�oA���A��A���A���A���A�z�A�dZA�x�A�=qA�ĜA���A���A�C�A�?}A���A��\A��yA�`BA�;dA�/A��yA�$�A�G�A�I�A�C�A�$�A��A�JA���A��`A��A��+A��9A�bA�XA�M�A���A�ffA��A�ȴA�ZA�r�A���A��A�^5A�\)A���A��A���A���A�Q�A��A�n�A�%A�p�A�M�A���A���A�"�A��A��jA���A��mA���A�5?A��yA�I�A�ZA�;dA��9A�z�A�{A��!A���A��A~E�A{�
Ax�yAv��At��AqK�AmoAj�jAhĜAeƨAc
=A`=qA^-A\��A\M�AY��AV~�AT9XAR1AO�wAO&�AN��AM�AL�+AK�hAH^5AG��AE��AD5?AB�jAAhsA?��A>��A=XA;�#A;p�A:ZA9XA8��A61'A57LA4�uA3%A2^5A1�#A0�uA/"�A.��A.�A-\)A+l�A((�A&9XA$��A#p�A!�mA�mA+A �AhsA^5A1AXA$�At�AVA��Ax�Av�AbAx�AVA��AdZA��AVAƨA��A�;AK�AJAZAdZA
��A	t�A��A��A	|�AS�A��AjAK�A��AQ�A%@�ƨ@���@��y@�
=@��!@��@��^@��/@�Z@�`B@�ff@�\@�^5@���@�33@���@��^@�Q�@��@�Z@�
=@��H@���@�Z@���@ޏ\@��
@ڗ�@��@ؼj@�1@�o@�E�@ղ-@��/@�Z@��y@�"�@���@�
=@�^5@·+@͡�@�Ĝ@��@��@�A�@��@�bN@��H@��@�~�@ř�@ċD@�\)@¸R@�-@�`B@�9X@��F@�dZ@��!@�E�@��T@���@��D@�S�@�5?@��7@�7L@���@�A�@��
@�l�@��^@��@�33@���@��H@��@���@�@��-@�V@�9X@���@�|�@��@��!@�-@��@��F@���@��@��T@���@�G�@�%@�z�@�
=@���@�ȴ@���@�J@��-@�X@�O�@��@�Z@� �@�1@��;@��@��P@�C�@�"�@�n�@�-@��@��^@���@��@�G�@��@��/@��D@��
@�|�@�o@��R@���@�n�@�E�@�$�@�J@�@���@��-@���@��@�`B@���@��9@��u@��D@�z�@�(�@���@�"�@�o@���@���@���@��+@�V@�{@��T@�@��h@�X@�/@���@��@�z�@�9X@���@��@�|�@�K�@�"�@��@��\@�$�@�@�7L@�%@���@��@�  @��@�ƨ@�ƨ@��@�\)@���@�^5@�{@���@�p�@�7L@��@���@���@�r�@� �@�ƨ@��@�+@��@�"�@�K�@��H@��!@���@��\@�v�@�@��#@��-@�X@��@��/@���@��D@�Z@�Z@��@��@�33@�33@���@��@�ȴ@�~�@�M�@��@���@��-@�/@�Q�@��w@���@�l�@�t�@���@��F@���@��
@��m@�ƨ@�|�@��H@�v�@�5?@��#@���@�`B@�/@��@��/@��j@���@��@��@�V@�Ĝ@��u@�r�@�Z@�9X@���@��w@��@���@���@���@�dZ@�dZ@�l�@��yG�O�@���@y�@r@�@k�@`�v@Z�x@S��@K��@H�U@C�a@;Z�@5��@01'@,4n@(��@"v�@�5@!-@��@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
E�B
u�B
z�B
�B
�JB
�hB
��B
��B
�B49B
�B
�B
�jB
��B
��B
�)B
�B
��BB
=BVB�B�B(�B8RBE�BJ�BcTBo�B�B�\B��B�B��B�JB�1B�bB�-B�
B��B��B	7B�B�B �B$�B?}BI�B?}B/B.B49B9XBJ�BdZBr�Bs�Br�Br�Bp�Bo�BjBffBe`Be`B`BBVBM�BE�B9XB#�B\B��B�HB��B��B�^B�FB�B�hBx�Bv�Bs�BdZB^5BO�BA�B/B�B
=B
��B
��B
��B
�B
�B
��B
�}B
��B
�DB
m�B
:^B
"�B
hB
B	�B	�fB	��B	�B	��B	�7B	v�B	]/B	I�B	8RB	.B	'�B	�B	+B��B�B�ZB�BB�5B�B��B��BÖB�jB�9B�B��B��B��B��B��B�bB�VB�\B�JB�JB�+B�B�B�B}�B|�B{�Bz�B}�B�B~�B�Bs�BgmBffBhsBgmBcTBaHB`BB`BB`BB_;B`BB^5B\)B[#B[#B^5B_;B_;BaHBbNB`BB_;BaHBdZBcTBe`BffBe`B^5BVBN�BN�BK�BH�BS�BcTBT�BM�BN�BI�B=qB<jBB�B:^B5?B2-B9XB=qB:^BE�BK�BN�BN�BG�BN�BN�BL�BL�BI�BH�BF�BB�BB�BG�B>wB8RB5?B49B1'B2-B33B33B33B33B49B7LB9XB<jB=qBD�BL�BL�BH�BF�BK�BL�BM�BP�BP�BQ�BT�B^5B`BB_;Bl�Bq�Br�Br�Bs�Bt�Bv�Bw�Bv�Bv�Bw�Bw�Bv�Bx�Bx�B~�B�B�B�%B�+B�1B�1B�1B�DB�7B�DB�DB�JB�hB�{B��B��B��B��B��B��B�B�B�!B�?B�RB�RB�XB�dB��BÖBĜBƨBƨBƨBǮBȴB��B��B��B��B�)B�BB�NB�TB�TB�fB�sB�B�B��B��B��B��B��B��B��B	B	B	%B	DB	bB	uB	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	'�B	'�B	(�B	.B	2-B	49B	49B	5?B	:^B	@�B	E�B	F�B	H�B	I�B	J�B	L�B	N�B	Q�B	S�B	T�B	VB	W
B	YB	^5B	aHB	cTB	ffB	iyB	m�B	o�B	r�B	s�B	v�B	y�B	|�B	~�B	�B	�B	�%B	�DB	�\B	�bB	�hB	�hB	�oB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�-B	�-B	�-B	�3B	�9B	�9B	�FB	�RB	�XB	�^B	�dB	�qB	�wB	�}B	ĜB	ĜB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�B	�B	�#B	�/B	�5B	�;B	�BB	�;B	�5B	�5B	�;B	�;B	�BB	�BB	�BB	�BB	�HB	�NB	�NB	�TB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B
�B
�B
�B
)*B
0�B
8�B
CGB
H�B
MB
R:B
X+B
\�B
`B
c�B
h�B
n/B
s�B
y>B
}�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B
�B
5�B
e�B
j�B
t!B
|QB
�nB
��B
��B
ہB$,B
�B
�B
�nB
��B
��B
�)B
�B
��B
�B
�8B
�QB{B
�B�B(HB5�B:�BSEB_�Bs BHB��B��B��B|8Bx B�PB�B��B��B��B�BeB�B�B�B/WB9�B/XB�B�B$B)4B:�BT1Bb�Bc�Bb�Bb�B`zB_uBZWBV>BU9BU9BPBE�B=�B5�B)8B�B�BB�B�5B��B�tB�PB�9B��B�`Bh�Bf�Bc�BTYBN5B?�B1�B"B�B
�IB
��B
�B
� B
�*B
�0B
�B
��B
�B
{bB
]�B
*�B
�B
�B	�8B	��B	֜B	�B	�YB	��B	y{B	gB	MyB	:B	(�B	eB	BB	
�B��B�:B��BԷBПBΓB�{B�LB�9B��B��B��B�tB�]B�3B�B��B��B��B~�B�B|�B|�Bw�Bt�BrzBquBncBm^BlWBkQBndBqvBojBqwBd)BW�BV�BX�BW�BS�BQ�BP�BP�BP�BO�BP�BN�BL�BK�BK�BN�BO�BO�BQ�BR�BP�BO�BQ�BT�BS�BU�BV�BU�BN�BF�B?WB?WB<FB94BDvBS�BE|B>RB?XB::B-�B,�B3B*�B%�B"�B)�B-�B*�B6%B<IB?[B?[B82B?\B?\B=PB=PB:>B98B7,B3B3B83B.�B(�B%�B$�B!�B"�B#�B#�B#�B#�B$�B'�B)�B,�B-�B5$B=TB=TB9<B70B<OB=UB>[BAmBAmBBtBE�BN�BP�BO�B]Bb.Bc4Bc5Bd;BeABgMBhSBgNBgNBhTBhTBgNBiZBiZBo~Bs�Bu�Bv�Bw�Bx�Bx�Bx�B{�By�B{�B{�B|�B��B��B�"B�4B�@B�XB�jB�qB��B��B��B��B��B��B��B��B�B�B�B�&B�&B�&B�,B�2B�WB�oB�oB�{B̥BоB��B��B��B��B��B�B�B�<B�BB�TB�`B�`B�gB�sB�~B��B��B��B	 �B	�B	�B	�B			B	B	B	'B	-B	@B	LB	dB	dB	jB	�B	"�B	$�B	$�B	%�B	*�B	0�B	6B	7B	9%B	:*B	;1B	==B	?IB	B\B	DgB	EmB	FsB	GyB	I�B	N�B	Q�B	S�B	V�B	Y�B	]�B	`
B	cB	d"B	g5B	jFB	mYB	oeB	t�B	u�B	v�B	{�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	� B	�2B	�8B	�>B	�DB	�WB	�hB	�nB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	� B	�B	�B	�0B	�6B	�6B	�0B	�6B	�BB	�ZB	�`B	�`B	�lB	�lB	�lB	�rB	�rB	�yB	˅B	͐B	ΖB	ϜB	УB	ϜB	ΖB	ΗB	ϜB	ϜB	УB	УB	УB	УB	ѩB	үB	үB	ӵB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�B	�G�O�B	�B	�4B
B
"B
�B
 �B
(�B
3�B
8�B
=tB
B�B
H�B
MB
PeB
S�B
Y3B
^�B
d'B
i�B
nB
pp111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.24 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.015(+/-0.005) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144272022020411442720220204114427  AO  ARCAADJP                                                                    20200619170931    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170931  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170931  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114427  IP                  G�O�G�O�G�O�                