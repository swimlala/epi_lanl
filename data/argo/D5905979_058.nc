CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:07Z creation      
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
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20200619170907  20220204114416  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               :A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ئ���W1   @ئ�}'�@6����o�c�O�;dZ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    :A   B   B   @�  @�  @���AffA>ffA`  A�  A�  A�  A���A�  A�  A�  A�  A�33B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/fD/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;fD;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DLfDL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy��D�fD�c�D���D�D�!HD�U�D���D���D��D�P�D�� DǦ�D�%qD�W
Dڗ
D��D�D�QHD�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�Q�@�Q�@��A�\A:�\A\(�A|(�A�{A�{A��HA�{A�{A�{A�{A�G�B
=B
=B
=B
=B'p�B/
=B7
=B?
=BG
=BO
=BW
=B_
=Bg
=Bo
=Bwp�B
=B��B��B��B��B��B��B�Q�B��B��B��B��B��B��B��B��B��BÅBǅB˅BϸRBӅBׅBۅB߅B�B�B�B�B�B��B��B��CCCCC	CCCCCCCCCCCC!C#C%C'C)C+C-C/C1C3C5C7C9C;C=C?CACCCECGCICKCMCOCQCSCUCWCYC[C]C_CaCcCe��CgCiCkCmCoCqCsCuCwCyC{C}CC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��{C��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��C��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��{C��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HD p�D �Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�D	p�D	�D
p�D
�Dp�D�Dj>D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�
Dw
D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�D p�D �D!p�D!�D"p�D"�D#p�D#�D$p�D$�D%p�D%�D&p�D&�D'p�D'�D(p�D(�D)p�D)�D*p�D*�D+p�D+�D,p�D,�D-p�D-�D.p�D.�
D/p�D/�D0p�D0�D1p�D1�D2p�D2�D3p�D3�D4p�D4�D5p�D5�D6p�D6�D7p�D7�D8p�D8�D9p�D9�D:p�D:�
D;p�D;�D<p�D<�D=p�D=�D>p�D>�D?p�D?�D@p�D@�DAp�DA�DBp�DB�DCp�DC�DDp�DD�DEp�DE�DFp�DF�DGp�DG�DHp�DH�DIp�DI�DJp�DJ�DKp�DK�
DLp�DL�DMp�DM�DNp�DN�DOp�DO�DPp�DP�DQp�DQ�DRp�DR�DSp�DS�DTp�DT�DUp�DU�DVp�DV�DWp�DW�DXp�DX�DYp�DY�DZp�DZ�D[p�D[�D\p�D\�D]p�D]�D^p�D^�D_p�D_�D`p�D`�Dap�Da�Dbp�Db�Dcp�Dc�Ddp�Dd�Dep�De�Dfp�Df�Dgp�Dg�Dhp�Dh�Dip�Di�Djp�Dj�Dkp�Dk�Dlp�Dl�Dmp�Dm�Dnp�Dn�Dop�Do�Dpp�Dp�Dqp�Dq�Drp�Dr�Dsp�Ds�Dtp�Dy|)D��D�[�D��>D���D��D�ND���D��)D�
D�IHD�xRDǟ
D��D�O\Dڏ\D�� D�fD�I�D�~fD��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��
A��;A��#A���A���A���A���A���A���A�  A�  A�  A�  A�A�A�A�A�A�A�%A�%A�1A�1A�
=A�
=A�JA�JA�JA�VA�VA�VA�bA�oA�{A�oA�oA�bA�bA�VA�bA�VA�{A�VA�bA�bA�VA�JA�JA���A���A���A�bNA���A�bA���A��^A���A���A�z�A�E�A��RA�ZA���A�z�A�ƨA��A��wA��A��A��^A� �A�"�A��A�%A�XA�E�A�$�A�A���A��wA�K�A���A�~�A��A�ffA��TA�E�A��9A�  A��yA�bNA�hsA�ZA�9XA�9XA��!A���A�p�A�%A��A��A�+A��RA�9XA��`A�M�A�1A�
=A��A�&�A���A��
A���A~M�A{��Av�DAu+At�!At��At�DAtz�AtbNAs�Ap��An�!Am`BAk�Aj�Ai?}Ag"�Ae`BAd-AdJAc�AaA`��A_�A^��A^ �A[��AXr�AW|�AV�RAU?}AT�DAT1AR��AQS�AQ
=AP�!AP  AN�/AN=qAM��AL�!AKp�AJE�AI�wAH~�AF-AC��AB��AB1AA+A@ �A?�wA>E�A<��A<jA<bA;oA9�;A8r�A7?}A5\)A4bA3�A2jA1x�A/�
A.�+A-�7A-
=A,ffA*�HA( �A&bNA& �A%l�A$^5A#7LA!��A�HA��A�A�A�RA�-A"�AM�Ap�A1A��A�A?}A��A�#A�
A/A��AffAƨA
�/A	��A	�A$�AĜAn�A��A��A�DAp�A ĜA $�@��@�o@���@��\@�M�@�x�@�^5@��T@��@��@�v�@�M�@�bN@�-@�7@��@��@蛦@��@�j@�+@���@��@��
@�5?@ܼj@�l�@��@��@؃@�dZ@�M�@ա�@��@�j@ӶF@ҧ�@�X@�t�@�33@�M�@��@�I�@���@�dZ@���@�E�@��@ɺ^@�p�@�/@�r�@��@��;@ǶF@��H@��T@�7L@ě�@��@�;d@���@°!@��@�hs@�Q�@��@���@��^@�1@���@��#@�X@���@�A�@��@�n�@�J@��@�z�@� �@�ƨ@�C�@�v�@���@���@�&�@���@��
@�\)@�"�@�^5@�O�@�%@�A�@��m@��w@�t�@���@���@��`@�z�@�I�@�  @��w@�|�@�"�@��R@��\@�n�@�^5@�$�@���@�G�@�9X@��;@��F@�l�@�ȴ@�ff@�5?@�@���@�p�@�p�@��@�j@��@��@��
@���@���@���@��;@��;@��m@�9X@�1'@�1@��
@���@��P@�l�@�"�@���@��H@���@�M�@��@��@�%@��@�1'@��;@��;@��m@���@��P@�t�@�dZ@�S�@��@�ff@���@�@�V@�bN@�1@���@�S�@��@�
=@���@���@���@�v�@�E�@�{@�{@��T@�`B@�%@��`@��j@��@�z�@�bN@�I�@��@�ƨ@���@�\)@�
=@���@�^5@��@���@�`B@�G�@���@��u@�j@�1@��F@�\)@�o@��@��@�ȴ@���@��+@�^5@�5?@���@��^@���@��7@�O�@��@�%@��`@���@��@�A�@� �@�ƨ@���@�dZ@��@��\@�-@��T@��^@�x�@�/@���@� �@�ƨ@��@�|�@�dZ@��@��y@���@��R@���@��+@�v�@�v�@�n�@�=q@�@�?}@�V@���@��@��/@�Ĝ@��@��u@�r�@�1'@�b@��;@�A�@z{�@rv�@eN<@`x@V^5@O��@I�-@D�E@<1@6��@2YK@*}V@&҉@"B[@#�@S�@��@|@��@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��
A��;A��#A���A���A���A���A���A���A�  A�  A�  A�  A�A�A�A�A�A�A�%A�%A�1A�1A�
=A�
=A�JA�JA�JA�VA�VA�VA�bA�oA�{A�oA�oA�bA�bA�VA�bA�VA�{A�VA�bA�bA�VA�JA�JA���A���A���A�bNA���A�bA���A��^A���A���A�z�A�E�A��RA�ZA���A�z�A�ƨA��A��wA��A��A��^A� �A�"�A��A�%A�XA�E�A�$�A�A���A��wA�K�A���A�~�A��A�ffA��TA�E�A��9A�  A��yA�bNA�hsA�ZA�9XA�9XA��!A���A�p�A�%A��A��A�+A��RA�9XA��`A�M�A�1A�
=A��A�&�A���A��
A���A~M�A{��Av�DAu+At�!At��At�DAtz�AtbNAs�Ap��An�!Am`BAk�Aj�Ai?}Ag"�Ae`BAd-AdJAc�AaA`��A_�A^��A^ �A[��AXr�AW|�AV�RAU?}AT�DAT1AR��AQS�AQ
=AP�!AP  AN�/AN=qAM��AL�!AKp�AJE�AI�wAH~�AF-AC��AB��AB1AA+A@ �A?�wA>E�A<��A<jA<bA;oA9�;A8r�A7?}A5\)A4bA3�A2jA1x�A/�
A.�+A-�7A-
=A,ffA*�HA( �A&bNA& �A%l�A$^5A#7LA!��A�HA��A�A�A�RA�-A"�AM�Ap�A1A��A�A?}A��A�#A�
A/A��AffAƨA
�/A	��A	�A$�AĜAn�A��A��A�DAp�A ĜA $�@��@�o@���@��\@�M�@�x�@�^5@��T@��@��@�v�@�M�@�bN@�-@�7@��@��@蛦@��@�j@�+@���@��@��
@�5?@ܼj@�l�@��@��@؃@�dZ@�M�@ա�@��@�j@ӶF@ҧ�@�X@�t�@�33@�M�@��@�I�@���@�dZ@���@�E�@��@ɺ^@�p�@�/@�r�@��@��;@ǶF@��H@��T@�7L@ě�@��@�;d@���@°!@��@�hs@�Q�@��@���@��^@�1@���@��#@�X@���@�A�@��@�n�@�J@��@�z�@� �@�ƨ@�C�@�v�@���@���@�&�@���@��
@�\)@�"�@�^5@�O�@�%@�A�@��m@��w@�t�@���@���@��`@�z�@�I�@�  @��w@�|�@�"�@��R@��\@�n�@�^5@�$�@���@�G�@�9X@��;@��F@�l�@�ȴ@�ff@�5?@�@���@�p�@�p�@��@�j@��@��@��
@���@���@���@��;@��;@��m@�9X@�1'@�1@��
@���@��P@�l�@�"�@���@��H@���@�M�@��@��@�%@��@�1'@��;@��;@��m@���@��P@�t�@�dZ@�S�@��@�ff@���@�@�V@�bN@�1@���@�S�@��@�
=@���@���@���@�v�@�E�@�{@�{@��T@�`B@�%@��`@��j@��@�z�@�bN@�I�@��@�ƨ@���@�\)@�
=@���@�^5@��@���@�`B@�G�@���@��u@�j@�1@��F@�\)@�o@��@��@�ȴ@���@��+@�^5@�5?@���@��^@���@��7@�O�@��@�%@��`@���@��@�A�@� �@�ƨ@���@�dZ@��@��\@�-@��T@��^@�x�@�/@���@� �@�ƨ@��@�|�@�dZ@��@��y@���@��R@���@��+@�v�@�v�@�n�@�=q@�@�?}@�V@���@��@��/@�Ĝ@��@��u@�r�@�1'@�bG�O�@�A�@z{�@rv�@eN<@`x@V^5@O��@I�-@D�E@<1@6��@2YK@*}V@&҉@"B[@#�@S�@��@|@��@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B�B@�BC�BD�BF�BK�BP�BVBhsBv�B}�B�B�B� B~�B~�B|�B~�B}�By�Bx�Bu�Bq�Bk�B_;BVBH�BB�B+B!�B�B�BhBJBB��B��B�fB�5B��B�dB�B��B��B�\B|�Bv�BgmBQ�BA�B<jB33B0!B$�B!�BoBB
�TB
ŢB
z�B
_;B
H�B
49B
�B
PB
JB
DB
JB
JB
JB
JB
\B
1B
B	��B	�B	�B	�5B	��B	��B	ȴB	ŢB	�dB	�3B	�B	��B	��B	��B	�B	y�B	u�B	n�B	iyB	ffB	`BB	XB	T�B	R�B	O�B	J�B	F�B	B�B	=qB	8RB	0!B	,B	%�B	�B	1B	B	  B��B��B�B�B�TB�HB�5B�B��B��BÖB�^B�-B�B��B��B��B�{B�VB�DB�1B� Bw�Bk�BiyBffB_;B[#B\)BP�BL�BE�BB�B=qB<jB;dB;dB;dB;dB=qB;dB9XB8RB:^B6FB5?B49B2-B1'B2-B0!B.B/B+B+B)�B)�B+B&�B'�B%�B%�B%�B%�B$�B#�B"�B$�B'�B#�B#�B#�B"�B&�B#�B#�B#�B$�B$�B$�B&�B&�B&�B&�B'�B'�B+B+B+B,B.B/B0!B1'B1'B2-B33B5?B7LB;dB:^B=qB@�BA�BB�BB�BF�BG�BH�BH�BI�BI�BM�BN�BP�BP�BS�BR�BT�BW
BYB^5B`BBbNBe`BiyBq�Bt�Bz�B�B�VB��B��B��B��B��B��B�B�B�'B�3B�9B�FB�RB�^B�qB�wB��BBƨBǮBȴB��B��B��B�B�B�B�B�/B�ZB�sB�B�B�B�B�B��B��B��B��B��B��B	  B	%B	\B	oB	{B	�B	�B	"�B	+B	0!B	2-B	33B	7LB	=qB	=qB	;dB	=qB	D�B	G�B	J�B	L�B	L�B	L�B	M�B	Q�B	VB	XB	[#B	]/B	_;B	aHB	dZB	e`B	gmB	iyB	m�B	p�B	s�B	y�B	|�B	� B	�B	�B	�B	�%B	�1B	�7B	�7B	�=B	�JB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�'B	�3B	�9B	�?B	�FB	�LB	�LB	�XB	�dB	�jB	�qB	�qB	�}B	B	B	ĜB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�)B	�/B	�5B	�;B	�BB	�HB	�TB	�`B	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�2B	��B
�B
 B
B
"�B
+QB
7�B
<�B
G�B
KB
PbB
R�B
W$B
\�B
`�B
h�B
l�B
poB
v`B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B�B8vB;�B<�B>�BC�BH�BM�B`dBn�Bu�B{By�Bw�Bv�Bv�Bt�Bv�Bu�Bq�Bp�Bm�Bi�BcyBW0BM�B@�B:�B"�B�B�B�B	cBEB�B��B�B�dB�4B��B�fB�B��B��B�aBt�Bn�B_uBI�B9�B4uB+?B(-B�B�B
~B
�B
�fB
��B
r�B
WVB
@�B
,XB
�B
rB
lB
fB
lB
lB
lB
lB
~B
 TB	�5B	�B	��B	�B	�[B	�%B	��B	��B	��B	��B	�]B	�8B	�B	��B	��B	{@B	r	B	m�B	f�B	a�B	^�B	XrB	PAB	M/B	K#B	HB	B�B	>�B	:�B	5�B	0�B	(UB	$=B	B	�B	 iB�JB�8B�'B�B��B��BۏBكB�pB�SB�.B�B��B��B�lB�GB�0B�B��B��B��B��B�sBxCBpBc�Ba�B^�BW�BSiBToBI,BEB=�B:�B5�B4�B3�B3�B3�B3�B5�B3�B1�B0�B2�B.�B-�B,�B*yB)sB*yB(mB&aB'hB#OB#OB"IB"JB#PB7B >B1B2B2B2B,B&B B,B ?B'B'B'B!B9B'B'B'B-B-B-B9B:B:B:B AB AB#SB#SB#SB$YB&eB'lB(rB)xB)xB*~B+�B-�B/�B3�B2�B5�B8�B9�B:�B:�B>�B?�BABABB
BB
BF#BG)BI5BI5BLHBKBBMNBOZBQgBV�BX�BZ�B]�Ba�Bi�BmBs/ByTB��B��B��B�B�B�B�<B�TB�ZB�sB�B��B��B��B��B��B��B��B��B��B��B��B�B�7B�=B�NB�ZB�ZB�aB�yBܤB�B��B��B��B��B��B�B�B�*B�0B�7B�=B�HB�mB	�B	
�B	�B	�B	�B	B	#HB	(gB	*sB	+yB	/�B	5�B	5�B	3�B	5�B	<�B	?�B	CB	EB	EB	EB	FB	J0B	NHB	PTB	SfB	UrB	W~B	Y�B	\�B	]�B	_�B	a�B	e�B	h�B	k�B	rB	u0B	xBB	}`B	}`B	}`B	~fB	�rB	�xB	�xB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�)B	�)B	�0B	�<B	�GB	�MB	�SB	�SB	�`B	�`B	�fB	�rB	�xB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�*B	�0B	�0B	�0B	�6B	�<B	�<B	�BB	�HB	�MB	�TB	�ZB	�fB	�lB	�rB	�xB	�B	مB	ۑB	ݝB	ߪB	ߪB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	�nB	��B
 9B

\B
KB
&B
#�B
/�B
5B
?�B
CIB
H�B
J�B
O]B
T�B
YB
`�B
e,B
h�B
n�B
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.24 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144162022020411441620220204114416  AO  ARCAADJP                                                                    20200619170907    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170907  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170907  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114416  IP                  G�O�G�O�G�O�                