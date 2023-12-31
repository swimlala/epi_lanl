CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:10Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170910  20220204114418  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               IA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ع\�+u1   @ع]��<@@6�ě��T�c���+1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    IA   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A���A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�33B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  Dy�D  D� D  Dy�D  D� D  D� D  Dy�D  D� D  D�fD  D� D  D� D  D� D  D� D��Dy�D  D�fD  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D*��D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DXfDX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_y�D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dny�Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy�D��D�P D��D�˅D�&D�_\D��qD��RD�#3D�UqD��)D��D�HD�Y�Dڑ�D��D�#�D�]D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�Q�@�Q�A(�A<(�A\(�A|(�A�{A�{A�{A��HA�{A�{A��HA�G�B
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
=B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BÅBǅB˅BυBӅBׅBۅB߅B�RB�RB�B�B�B��RB��B��CCCCC	CCCCCCCCCCCC!C#C%C'C)C+C-C/C1C3C5C7C9C;C=C?CACCCECGCICKCMCOCQCSCUCWCYC[C]C_CaCcCeCgCiCkCmCoCqCsCuCwCyC{C}CC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��{C��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��{C��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��C��HC��HC��HC��HC��HC��HC��HC��HC��HD p�D �Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�D	p�D	�D
p�D
�Dp�D�Dp�D�Dj>D�Dp�D�Dj>D�Dp�D�Dp�D�Dj>D�Dp�D�Dw
D�Dp�D�Dp�D�Dp�D�Dp�D�>Dj>D�Dw
D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�D p�D �D!p�D!�D"p�D"�D#p�D#�D$p�D$�D%p�D%�D&p�D&�D'p�D'�D(p�D(�D)p�D)�D*p�D*�>D+p�D+�D,p�D,�D-p�D-�D.p�D.�D/p�D/�D0p�D0�D1p�D1�D2p�D2�D3p�D3�D4p�D4�D5p�D5�D6p�D6�D7p�D7�D8p�D8�D9p�D9�D:p�D:�D;p�D;�D<p�D<�D=p�D=�D>p�D>�D?p�D?�D@p�D@�DAp�DA�DBp�DB�DCp�DC�DDp�DD�DEp�DE�DFp�DF�DGp�DG�DHp�DH�DIp�DI�DJp�DJ�DKp�DK�DLp�DL�DMp�DM�DNp�DN�DOp�DO�DPp�DP�DQp�DQ�DRp�DR�DSp�DS�DTp�DT�DUp�DU�DVp�DV�DWp�DW�
DXp�DX�DYp�DY�DZp�DZ�D[p�D[�D\p�D\�D]p�D]�D^p�D^�D_j>D_�D`p�D`�Dap�Da�Dbp�Db�Dcp�Dc�Ddp�Dd�Dep�De�Dfp�Df�Dgp�Dg�Dhp�Dh�Dip�Di�Djp�Dj�Dkp�Dk�Dlp�Dl�Dmp�Dm�Dnj>Dn�Dop�Do�Dpp�Dp�Dqp�Dq�Drp�Dr�Dsp�Ds�Dtp�Dt��Dy��D��D�HRD��qD���D�fD�W�D���D���D��D�M�D��{D��fD�	�D�R>Dڊ>D���D��D�UqD��D��>111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�p�A�z�A�|�AȅAȇ+Aȇ+Aȇ+A�~�A�hsA�9XA��A�oA�bA���Aǣ�AŮA��A��
A�z�A��A�G�A�VA��A��/A��A�hsA��9A���A�^5A���A��/A�=qA�|�A�I�A�"�A�XA��RA�%A�~�A�VA�p�A�`BA�"�A���A��wA�A�A��HA��hA�;dA��jA��DA�v�A�O�A��FA��;A�+A���A�7LA���A���A�`BA��jA���A��TA�ĜA�n�A���A���A��uA�{A��DA�/A�z�A��A�ĜA�ĜA��A��A�
=A�E�A�+A��FA���A��mA���A��;A�^5A�l�A��A��A�{A�  A�  A��RA�XA���A�/A�;dA�A�A�M�A��
A��uA�Q�A�oA��wA���A�VA�l�A�ĜA�A�5?A��^A��A��wA��FA�x�A�E�A��A}`BA{�-Az�+Ax$�Av��Au\)AsC�Ap�HAn$�Ak�PAj�Ai�Ad��Ab��Aa�A`A^ �A\{A[/AZ��AZ$�AY+AW?}AU��ATZAR�/APA�ANA�ALJAK�AJ�!AI�AH1'AFA�AD�/AC�FAB��AAA?�TA?K�A>��A>(�A=;dA;�
A9�A9�A8bNA7�-A7
=A6��A6v�A5�A3+A1l�A0M�A.�A-�;A-A-p�A+��A)��A)�
A)t�A(�\A'��A&A$�A$~�A$n�A$ �A#;dA"bA   A�A�AVA�A��A�;AI�A��Ap�AVA^5A�FA�A��AA\)A�uA�A�RAv�A=qA�mAO�A�#A
bNA
E�A	��A	l�A�A�DA�HAQ�A��A�/Ar�A�FA�hAl�A33A �H@�-@��9@�1@�
=@��7@�A�@��!@�?}@��@�hs@��/@��@��/@웦@�ȴ@�@�+@��@� �@�@�!@�`B@�hs@��@���@�t�@�
=@�=q@�&�@��H@�-@�@�V@��@���@�$�@��@�b@�l�@�n�@���@�x�@��@υ@Ͼw@���@ѡ�@���@�1@϶F@ϥ�@ϕ�@�"�@�r�@�S�@�-@Ɂ@Ȭ@�ƨ@�S�@��@�Ĝ@ě�@�r�@��@��
@�ƨ@�=q@�$�@�p�@���@��y@���@��\@�`B@�1@��!@��@���@���@�&�@�%@��m@��h@��w@��@�J@��D@�
=@�=q@���@��h@�`B@��u@��;@�
=@���@���@��9@��@�ƨ@�S�@���@���@��\@�V@��T@��-@�?}@��`@��D@�(�@��w@�S�@�=q@�@���@�bN@�b@��;@���@��@��!@�5?@�$�@��@�?}@��@�V@���@���@�(�@��F@�;d@�;d@�33@�33@�+@�"�@�o@��@���@�V@�J@��@��-@�p�@�O�@���@��m@��w@�l�@��@��R@�~�@�^5@�=q@�J@��-@��@�G�@�V@��`@��j@���@��D@�r�@�bN@�Q�@��@�dZ@��H@���@���@�ff@�ff@�ff@�^5@�^5@�=q@�J@��T@���@���@�O�@��@�j@��
@�ƨ@��@��@���@���@�5?@�$�@�n�@�ff@�v�@��+@�n�@�=q@��@���@��7@�`B@�G�@�&�@�I�@�  @�1@��m@��@��P@�C�@�o@�o@��@��!@��!@�ȴ@���@�~�@�^5@�V@���@�x�@�hs@�7L@��@�V@�V@���@���@��j@��9@���@��@�Q�@��@��m@�ƨ@���@�|�@�S�@�"�@��@�o@�o@�o@�@��!@���@�n�@�^5@�J@�@���@��7@��@|��@t9X@j�2@_iD@Wn/@Q��@IA @A#�@:i�@63�@/�@*d�@#�k@^5@�e@c�@�M@w2@	��@M111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�p�A�z�A�|�AȅAȇ+Aȇ+Aȇ+A�~�A�hsA�9XA��A�oA�bA���Aǣ�AŮA��A��
A�z�A��A�G�A�VA��A��/A��A�hsA��9A���A�^5A���A��/A�=qA�|�A�I�A�"�A�XA��RA�%A�~�A�VA�p�A�`BA�"�A���A��wA�A�A��HA��hA�;dA��jA��DA�v�A�O�A��FA��;A�+A���A�7LA���A���A�`BA��jA���A��TA�ĜA�n�A���A���A��uA�{A��DA�/A�z�A��A�ĜA�ĜA��A��A�
=A�E�A�+A��FA���A��mA���A��;A�^5A�l�A��A��A�{A�  A�  A��RA�XA���A�/A�;dA�A�A�M�A��
A��uA�Q�A�oA��wA���A�VA�l�A�ĜA�A�5?A��^A��A��wA��FA�x�A�E�A��A}`BA{�-Az�+Ax$�Av��Au\)AsC�Ap�HAn$�Ak�PAj�Ai�Ad��Ab��Aa�A`A^ �A\{A[/AZ��AZ$�AY+AW?}AU��ATZAR�/APA�ANA�ALJAK�AJ�!AI�AH1'AFA�AD�/AC�FAB��AAA?�TA?K�A>��A>(�A=;dA;�
A9�A9�A8bNA7�-A7
=A6��A6v�A5�A3+A1l�A0M�A.�A-�;A-A-p�A+��A)��A)�
A)t�A(�\A'��A&A$�A$~�A$n�A$ �A#;dA"bA   A�A�AVA�A��A�;AI�A��Ap�AVA^5A�FA�A��AA\)A�uA�A�RAv�A=qA�mAO�A�#A
bNA
E�A	��A	l�A�A�DA�HAQ�A��A�/Ar�A�FA�hAl�A33A �H@�-@��9@�1@�
=@��7@�A�@��!@�?}@��@�hs@��/@��@��/@웦@�ȴ@�@�+@��@� �@�@�!@�`B@�hs@��@���@�t�@�
=@�=q@�&�@��H@�-@�@�V@��@���@�$�@��@�b@�l�@�n�@���@�x�@��@υ@Ͼw@���@ѡ�@���@�1@϶F@ϥ�@ϕ�@�"�@�r�@�S�@�-@Ɂ@Ȭ@�ƨ@�S�@��@�Ĝ@ě�@�r�@��@��
@�ƨ@�=q@�$�@�p�@���@��y@���@��\@�`B@�1@��!@��@���@���@�&�@�%@��m@��h@��w@��@�J@��D@�
=@�=q@���@��h@�`B@��u@��;@�
=@���@���@��9@��@�ƨ@�S�@���@���@��\@�V@��T@��-@�?}@��`@��D@�(�@��w@�S�@�=q@�@���@�bN@�b@��;@���@��@��!@�5?@�$�@��@�?}@��@�V@���@���@�(�@��F@�;d@�;d@�33@�33@�+@�"�@�o@��@���@�V@�J@��@��-@�p�@�O�@���@��m@��w@�l�@��@��R@�~�@�^5@�=q@�J@��-@��@�G�@�V@��`@��j@���@��D@�r�@�bN@�Q�@��@�dZ@��H@���@���@�ff@�ff@�ff@�^5@�^5@�=q@�J@��T@���@���@�O�@��@�j@��
@�ƨ@��@��@���@���@�5?@�$�@�n�@�ff@�v�@��+@�n�@�=q@��@���@��7@�`B@�G�@�&�@�I�@�  @�1@��m@��@��P@�C�@�o@�o@��@��!@��!@�ȴ@���@�~�@�^5@�V@���@�x�@�hs@�7L@��@�V@�V@���@���@��j@��9@���@��@�Q�@��@��m@�ƨ@���@�|�@�S�@�"�@��@�o@�o@�o@�@��!@���@�n�@�^5@�J@�@���G�O�@��@|��@t9X@j�2@_iD@Wn/@Q��@IA @A#�@:i�@63�@/�@*d�@#�k@^5@�e@c�@�M@w2@	��@M111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
��B
��B
��B
�B
�)B
�TB
�yB
�B
�B
�B
�B
�B�B<jBaHB�1B�{B�B�B�B�B�-B�LBǮB�#B�
B�
B��B�TB�B�B��BbB�B�B$�B)�B49B>wBE�BN�B[#BhsB�%B��B��B�B�B�B�B�B��B�VB�Bu�BbNBl�B|�B�+B�bB��B��B��B��B��B��B��B��B�uB�VB�1B�Bn�Bp�Bm�Bo�B�B�Bt�B|�B�1B~�BiyBR�BE�B2-B&�B)�B,B.B)�B!�B%�B/BD�B`BBM�BB�B<jB8RBI�BYBC�BDB�B�-B�uBYB0!BPB
�sB
ǮB
��B
��B
� B
e`B
ZB
N�B
A�B
49B
-B
%�B
�B
	7B	��B	�B	�fB	��B	�wB	�9B	�!B	��B	��B	��B	��B	�oB	�VB	�B	y�B	q�B	iyB	_;B	T�B	F�B	A�B	=qB	8RB	/B	"�B	�B	\B		7B	  B��B�B�B�B�mB�HB�B��B��B��B�B�
B��B��B��BɺBÖB��B�^B�RB�LB�9B�B��B��B��B��B��B��B��B��B�uB�\B�+B�B~�B{�Bw�Bs�Bp�Bm�BhsBffBdZBbNB`BB^5B[#BZBS�BS�BR�BO�BN�BM�BL�BK�BI�BK�BF�BF�BF�BG�BF�BG�BE�BA�B?}B@�BD�BA�BB�BD�BG�BL�BVBR�BR�BQ�BS�BR�BS�BR�BYBW
BXBZB[#B[#B^5BaHBgmBgmBe`BffBffBjBp�Bv�B}�B|�B�B�B�B�7B�=B�DB�PB�VB�VB�bB�hB�bB�oB�uB�uB�{B��B��B��B��B�!B�FB�FB�?B�?B�?B�LB�dB�jB��B��B�}B��B�wB�}B�}B�}B�}B�}B�wB�qBBȴB��B��B��B��B��B��B��B��BǮBƨBǮBƨBŢB��B��B��B��B�
B�B�)B�;B�HB�TB�ZB�sB�B�B�B��B��B	B	B	B	+B	1B	1B		7B	PB	VB	hB	uB	�B	�B	�B	�B	&�B	)�B	/B	49B	6FB	7LB	9XB	=qB	?}B	D�B	D�B	G�B	L�B	M�B	M�B	M�B	P�B	S�B	XB	\)B	]/B	]/B	]/B	]/B	]/B	^5B	_;B	dZB	hsB	jB	k�B	l�B	m�B	m�B	q�B	u�B	u�B	w�B	y�B	|�B	}�B	~�B	~�B	�B	�B	�+B	�1B	�=B	�DB	�JB	�PB	�PB	�PB	�PB	�VB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�-B	�3B	�9B	�?B	�FB	�^B	�jB	�}B	��B	��B	��B	��B	��B	��B	B	B	��B	�}B	��B	��B	B	B	ŢB	ŢB	ŢB	ƨB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�)B	�)B	�/B	�/B	�bB	�+B
B
�B
VB
)*B
/�B
88B
?cB
F�B
J=B
R�B
W�B
]�B
c:B
g�B
k�B
n�B
shB
w�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�CB
�CB
�CB
�IB
�OB
�UB
�[B
ԀB
۪B
��B
��B
��B
��B
��B
�B�B4�BY�B�B��B�NB�ZB�`B�`B�yB��B��B�mB�TB�TB�*B۞B��B��B�B�B�B�B$B"CB,B6�B=�BGBSgB`�B~gB��B�B�HB�BB�BB�HB�HB��B��B{UBnBZ�Bd�Bu2BnB��B��B��B��B� B� B��B��B��B��B��B�uB{VBf�Bh�Be�Bg�B|^B{WBmBu3B�uBw?Ba�BK:B=�B*wB4B"GB$SB&_B"GBB/B'fB<�BX�BFB:�B4�B0�BBBQ`B;�B�B�iB��B��BQrB(B�B
��B
�B
�cB
�&B
xjB
]�B
R�B
GGB
9�B
,�B
%~B
TB
B
�B	�DB	�B	��B	�9B	��B	��B	��B	�dB	�9B	�B	�B	��B	��B	z�B	rYB	j)B	a�B	W�B	MB	?+B	:B	5�B	0�B	'�B	VB	B	�B	�B��B�XB�?B�-B�B��B��BЛB�xB�kB�~BМBϖB͊B͊B�lB�GB�$B�B��B��B��B��B��B��B��B�mB�bB�CB�%B�B�B�B��B�B|�Bw�Bt{BpcBlKBi9Bf'Ba	B^�B\�BZ�BX�BV�BS�BR�BL�BL�BK�BHxBGrBFlBEfBD`BBSBD`B?BB?BB?BB@HB?BB@HB><B:$B8B9B=7B:$B;*B=7B@IBEhBN�BK�BK�BJ�BL�BK�BL�BK�BQ�BO�BP�BR�BS�BS�BV�BY�B`B`B]�B_B_BcBi?BodBv�Bu�By�B|�B}�B��B��B��B��B��B��B��B�B��B�	B�B�B�B�!B�:B�LB��B��B��B��B��B��B��B��B��B�B�!B�!B�B�B�B�B�B�B�B�B�B�
B�(B�LB�YB�qB�kB�fB�fB�lB�`B�ZB�GB�AB�GB�AB�;B�xB�rBʅB̑BϣBҶB��B��B��B��B��B�B�B�<B�HB�mB�B��B��B��B��B	 �B	 �B	�B	�B	�B		�B	B	B	)B	<B	NB	B	"�B	'�B	,�B	.�B	/�B	1�B	6B	8B	=0B	=0B	@BB	EaB	FgB	FgB	FgB	IyB	L�B	P�B	T�B	U�B	U�B	U�B	U�B	U�B	V�B	W�B	\�B	aB	cB	dB	eB	f#B	f#B	j<B	nUB	nUB	paB	rmB	u�B	v�B	w�B	w�B	y�B	}�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�*B	�*B	�*B	�*B	�*B	�0B	�6B	�<B	�<B	�BB	�NB	�UB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�0B	�0B	�0B	�6B	�6B	�<B	�BB	�OB	�OB	�OB	�[B	�[B	�[B	�aB	�aB	�gB	�gB	�gB	�gB	�mB	�sB	�sB	�zB	�sB	�zB	̅B	ϗB	НB	НB	ѤB	ѤB	ҪB	ҪB	ҪB	ҪB	ҪB	ҪB	ӰB	ӰB	ӰB	ӰB	ԶB	ԶB	ռG�O�B	��B	�B	��B

(B
�B
!�B
((B
0�B
7�B
?B
B�B
KB
PKB
V;B
[�B
`]B
dB
gnB
k�B
pWB
to111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.24 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.007(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144182022020411441820220204114418  AO  ARCAADJP                                                                    20200619170910    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170910  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170910  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114418  IP                  G�O�G�O�G�O�                