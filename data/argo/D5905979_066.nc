CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-18T14:13:54Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200618141354  20220204114417  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               BA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ذ��1   @ذ�����@6e�S����d�O�;d1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    BA   B   B   @�33@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv�Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU�fDV  DV� DW  DW� DXfDX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr�fDs  Ds� Dt  Dt� Dt��Dy� D�\D�Z=D���D�ٚD�#�D�\�D��\D��3D�'
D�c3D���D��D��D�O\D�~�D�h�D�"=D�X�D��D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@�RA��A8��AX��Ax��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B�B=qB=qB=qB&=qB.=qB6=qB>=qBF=qBN=qBV=qB^=qBf=qBn=qBv=qB~=qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�\C�\C�\C�\C	�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C!�\C#�\C%�\C'�\C)�\C+�\C-�\C/�\C1�\C3�\C5�\C7�\C9�\C;�\C=�\C?�\CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO�\CQ�\CS�\CU�\CW�\CY�\C[�\C]�\C_�\Ca�\Cc�\Ce�\Cg�\Ci�\Ck�\Cm�\Co�\Cq�\Cs��Cu��Cw�\Cy�\C{�\C}�\C�\C�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC���C�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC��{C�ǮC�ǮC��C�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮD c�D ��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��D	c�D	��D
c�D
��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dj=D��Dc�D��Dc�D��Dc�D��D c�D ��D!c�D!��D"c�D"��D#c�D#��D$c�D$��D%c�D%��D&c�D&��D'c�D'��D(c�D(��D)c�D)��D*c�D*��D+c�D+��D,c�D,��D-c�D-��D.c�D.��D/c�D/��D0c�D0��D1c�D1��D2c�D2��D3c�D3��D4c�D4��D5c�D5��D6c�D6��D7c�D7��D8c�D8��D9c�D9��D:c�D:��D;c�D;��D<c�D<��D=c�D=��D>c�D>��D?c�D?��D@c�D@��DAc�DA��DBc�DB��DCc�DC��DDc�DD��DEc�DE��DFc�DF��DGc�DG��DHc�DH��DIc�DI��DJc�DJ��DKc�DK��DLc�DL��DMc�DM��DNc�DN��DOc�DO��DPc�DP��DQc�DQ��DRc�DR��DSc�DS��DTc�DT��DUj=DU��DVc�DV��DWc�DW�=DXc�DX��DYc�DY��DZc�DZ��D[c�D[��D\c�D\��D]c�D]��D^c�D^��D_c�D_��D`c�D`��Dac�Da��Dbc�Db��Dcc�Dc��Ddc�Dd��Dec�De��Dfc�Df��Dgc�Dg��Dhc�Dh��Dic�Di��Djc�Dj��Dkc�Dk��Dlc�Dl��Dmc�Dm��Dnc�Dn��Doc�Do��Dpc�Dp��Dqc�Dq��Drj=Dr��Dsc�Ds��Dtc�Dt��Dys�D�HD�L)D���D�ˆD��D�N�D��HD��D��D�UD���D�� D��D�AHD�p�D�Z�D�)D�J�D��D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�|�A�|�A�~�A��DA��\A��hA��uA���A���A��\A�ZA�{A��A�1'A�$�A��A���A��mA��A���A�ĜA��RA���A��uA���A���A���A���A���A�ffA�;dA��A���A�=qA�5?A�5?A�5?A�oA���A��A�bA��`A���A�A���A�x�A�C�A�9XA��A��PA���A�(�A��A��7A�S�A��A��/A�C�A��A�bNA���A��jA�(�A�|�A�E�A���A�/A���A�K�A���A���A� �A��RA�n�A�oA�^5A��jA�+A��A�Q�A�&�A�7LA��#A��A�+A��A��#A�t�A��#A�1'A��7A���A�+A�VA�A��A�VA���A�G�A�/A�&�A�Q�A���A��A���A��PA�K�A�+A��FA�-A�x�A���A}hsAy�
Aw
=At�jAs"�Ap�9Ak�Aip�Ag
=Ad-Aa\)A^ĜA\��A\r�A[�AZQ�AX��AVM�ATv�AS�ARn�AO;dAM�AK?}AKAJ��AJ��AH��AG�AE�;AEO�AD=qACC�AC;dAC33AB�DAAAA�AA&�A@{A>��A=hsA:�uA7hsA533A3S�A2�RA0�HA01'A-"�A+ƨA)|�A'�PA&��A&�A%&�A#��A#�hA#S�A"�A"�uA"�A!�-A!�A �HA �!A ��A ��A z�A Q�A�At�A��AA��A�A\)AȴA��A��A��AoA��A�9A�`A��A��Av�AO�A
1'At�AVA�AVA{A��AA��A �@�E�@���@� �@�C�@��H@�-@��@�r�@��H@��+@�w@��m@�P@�P@�@�\)@�K�@�o@��@���@�bN@�n�@�I�@���@�A�@◍@�hs@���@�r�@߶F@�n�@�G�@�bN@�K�@���@��H@ڟ�@��T@�%@�bN@�ƨ@�33@��@�{@�V@�Q�@��;@ӍP@�~�@�?}@мj@ϕ�@��@͑h@�X@��@�Ĝ@�A�@�+@ʟ�@�@�V@�bN@ǥ�@�
=@���@Ɵ�@��#@���@�bN@ÍP@�
=@��H@���@���@§�@�{@��@��@�n�@�5?@��-@���@��;@��P@�\)@��@��!@�@�X@��9@��D@�9X@�b@��@�@��+@�@��^@�hs@�7L@���@�9X@��w@�E�@��@��h@�X@�%@��D@� �@��@���@�;d@�@�hs@��9@�j@��@���@�@�?}@��u@�1@��F@�t�@�"�@���@���@�M�@��@�@�p�@�/@�V@���@�r�@��;@��P@��@�M�@�@�p�@��@�%@��`@�z�@��@�S�@�
=@��R@�M�@�@���@�/@���@��@�1@�ƨ@���@�dZ@�K�@���@���@��+@�-@��^@��h@�/@��@��@��9@�1'@�  @�A�@�I�@��m@�o@���@�E�@�{@��^@�7L@��`@�(�@�(�@�(�@�;d@��R@�ȴ@��@��@��+@�^5@���@��^@���@�hs@�hs@�hs@�X@�V@��@��@��@��`@��9@��@��@�(�@�1@��@�dZ@�+@��!@�~�@�M�@��@���@��@��T@���@���@�x�@��@���@�j@��m@��w@���@�S�@��@��R@���@���@�n�@�M�@�-@�J@�@��@���@���@�p�@�?}@�/@��@�r�@�Q�@�9X@�b@�ƨ@���@��P@�S�@�K�@�;d@�C�@�;d@�;d@�C�@�K�@�;d@�@���@�ff@�=q@��@�p�@��@���@��@���@�n�@�E�@��@��@��#@�`B@yIR@p�o@f�1@^c @W&@OdZ@J�@B�6@<u�@3E9@,�z@'�f@#l�@�@��@�.@o @�@�*@ݘ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�|�A�|�A�~�A��DA��\A��hA��uA���A���A��\A�ZA�{A��A�1'A�$�A��A���A��mA��A���A�ĜA��RA���A��uA���A���A���A���A���A�ffA�;dA��A���A�=qA�5?A�5?A�5?A�oA���A��A�bA��`A���A�A���A�x�A�C�A�9XA��A��PA���A�(�A��A��7A�S�A��A��/A�C�A��A�bNA���A��jA�(�A�|�A�E�A���A�/A���A�K�A���A���A� �A��RA�n�A�oA�^5A��jA�+A��A�Q�A�&�A�7LA��#A��A�+A��A��#A�t�A��#A�1'A��7A���A�+A�VA�A��A�VA���A�G�A�/A�&�A�Q�A���A��A���A��PA�K�A�+A��FA�-A�x�A���A}hsAy�
Aw
=At�jAs"�Ap�9Ak�Aip�Ag
=Ad-Aa\)A^ĜA\��A\r�A[�AZQ�AX��AVM�ATv�AS�ARn�AO;dAM�AK?}AKAJ��AJ��AH��AG�AE�;AEO�AD=qACC�AC;dAC33AB�DAAAA�AA&�A@{A>��A=hsA:�uA7hsA533A3S�A2�RA0�HA01'A-"�A+ƨA)|�A'�PA&��A&�A%&�A#��A#�hA#S�A"�A"�uA"�A!�-A!�A �HA �!A ��A ��A z�A Q�A�At�A��AA��A�A\)AȴA��A��A��AoA��A�9A�`A��A��Av�AO�A
1'At�AVA�AVA{A��AA��A �@�E�@���@� �@�C�@��H@�-@��@�r�@��H@��+@�w@��m@�P@�P@�@�\)@�K�@�o@��@���@�bN@�n�@�I�@���@�A�@◍@�hs@���@�r�@߶F@�n�@�G�@�bN@�K�@���@��H@ڟ�@��T@�%@�bN@�ƨ@�33@��@�{@�V@�Q�@��;@ӍP@�~�@�?}@мj@ϕ�@��@͑h@�X@��@�Ĝ@�A�@�+@ʟ�@�@�V@�bN@ǥ�@�
=@���@Ɵ�@��#@���@�bN@ÍP@�
=@��H@���@���@§�@�{@��@��@�n�@�5?@��-@���@��;@��P@�\)@��@��!@�@�X@��9@��D@�9X@�b@��@�@��+@�@��^@�hs@�7L@���@�9X@��w@�E�@��@��h@�X@�%@��D@� �@��@���@�;d@�@�hs@��9@�j@��@���@�@�?}@��u@�1@��F@�t�@�"�@���@���@�M�@��@�@�p�@�/@�V@���@�r�@��;@��P@��@�M�@�@�p�@��@�%@��`@�z�@��@�S�@�
=@��R@�M�@�@���@�/@���@��@�1@�ƨ@���@�dZ@�K�@���@���@��+@�-@��^@��h@�/@��@��@��9@�1'@�  @�A�@�I�@��m@�o@���@�E�@�{@��^@�7L@��`@�(�@�(�@�(�@�;d@��R@�ȴ@��@��@��+@�^5@���@��^@���@�hs@�hs@�hs@�X@�V@��@��@��@��`@��9@��@��@�(�@�1@��@�dZ@�+@��!@�~�@�M�@��@���@��@��T@���@���@�x�@��@���@�j@��m@��w@���@�S�@��@��R@���@���@�n�@�M�@�-@�J@�@��@���@���@�p�@�?}@�/@��@�r�@�Q�@�9X@�b@�ƨ@���@��P@�S�@�K�@�;d@�C�@�;d@�;d@�C�@�K�@�;d@�@���@�ff@�=q@��@�p�@��@���@��@���@�n�@�E�@��@��G�O�@�`B@yIR@p�o@f�1@^c @W&@OdZ@J�@B�6@<u�@3E9@,�z@'�f@#l�@�@��@�.@o @�@�*@ݘ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBƨBƨBŢBŢBĜBŢBŢBŢBŢBŢBŢBŢBƨB��B��B��B��B��B��B�B�)B�;B�yB�B��B��B��B+BuB�B+B1'B33B5?B:^BA�BF�BF�BL�BQ�BhsB�hB��B�B�-B�XB�dB�RB�^B�jB�^B�3B��B�\B�DB�JB�=B�PBv�Bn�Bm�Be`B]/BXBXBO�BE�B;dB33B2-B6FB,BuBPBB�B�B�
B��B�RB�B��B��B�7B{�B\)BbB
��B
��B
��B
�B
ǮB
�qB
�-B
��B
��B
��B
��B
�DB
m�B
[#B
8RB
�B
%B	��B	�sB	�;B	��B	�jB	��B	��B	�DB	� B	t�B	jB	gmB	bNB	[#B	S�B	D�B	?}B	5?B	1'B	'�B	�B	oB	\B	PB	DB	%B��B��B�B�B�mB�fB�`B�TB�/B�#B�B��B��B��B�FB��B��B�hB�JB�%B�By�Bp�Bp�Bk�BhsBffBe`BbNBaHB`BB`BB_;B^5B\)B\)B[#BZBYBYBYBW
BT�BR�BM�BI�BK�BG�BH�BG�BF�BG�BD�BD�BD�BC�BA�BF�BB�BC�BA�B?}B=qB7LB7LB49B49B33B2-B2-B49B1'B/B0!B/B/B/B0!B1'B1'B/B33B2-B33B33B49B5?B49B49B49B33B1'B0!B33BA�BR�BVBXBYBZB]/B`BB`BBaHBcTBdZBcTBdZBdZBe`BffBhsBiyBjBjBjBk�BjBl�Bo�Bo�Bm�Bo�Bs�Br�Br�Bs�Br�Bt�Bv�Bw�Bx�B{�B|�B~�B�B� B�B�B�+B�1B�=B�DB�JB�DB�DB�DB�JB�oB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�-B�9B�FB�LB�XB�XB�^B�qB�}BƨBǮBȴBɺB��B��B��B��B��B��B�
B�B�5B�;B�HB�`B�yB�B�B��B��B��B��B	  B	B	B	%B	+B	
=B	PB	VB	\B	oB	�B	�B	�B	$�B	)�B	,B	.B	.B	/B	2-B	6FB	:^B	;dB	>wB	A�B	C�B	E�B	H�B	J�B	L�B	P�B	R�B	T�B	XB	[#B	^5B	_;B	`BB	aHB	cTB	dZB	ffB	hsB	hsB	jB	n�B	t�B	y�B	|�B	}�B	�B	�+B	�1B	�+B	�+B	�1B	�DB	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�3B	�9B	�?B	�FB	�LB	�LB	�XB	�jB	�wB	��B	B	B	ÖB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�B	�B	�/B	�/B	�/B	�/B	�/B	�5B	�5B	�5B	�;B	�;B	�;B	�BB	�BB	�HB	�HB	�TB	�TB	�ZB	�`B	�`B	�fB	�mB	�sB	�sB	�`B	�`B	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�QB	�wB
�B
eB
!B
*eB
2aB
6zB
?.B
EB
MB
S�B
XB
[�B
a-B
e,B
jKB
o�B
tTB
y�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�B�#B�#B�GB�_B�qB�B��B�B�B�)B�_B�B�B#4B)YB+eB-qB2�B9�B>�B>�BD�BJB`�B��B��B�?B�XB��B��B�}B��B��B��B�_B�
B��B�sB�yB�lB�Bn�Bf�Be�B]�BUaBPCBPCBHB=�B3�B+iB*cB.{B$>B�B�B�XB��B�TB�GB�#B��B�CB�B��B�{Bt,BTpB�B
�HB
�#B
�B
��B
� B
��B
��B
�PB
�7B
�B
��B
��B
e�B
S}B
0�B
B	��B	�%B	��B	ןB	�3B	��B	�LB	�B	��B	xmB	m*B	b�B	_�B	Z�B	S�B	LhB	=B	7�B	-�B	)�B	 dB	!B	
�B	�B	�B	�B��B�lB�HB�)B�B��B��B��B��BժBӞBђB�hB�=B�B��B�]B�B��B��B~�By�Br_Bi)Bi)BdB`�B^�B]�BZ�BY�BX�BX�BW�BV�BT�BT�BS�BR�BQ�BQ�BQ�BO�BM�BK{BF]BBDBDRB@9BA?B@9B?3B@:B=(B=(B=(B<"B:B?4B;B<#B:B8B5�B/�B/�B,�B,�B+�B*�B*�B,�B)�B'�B(�B'�B'�B'�B(�B)�B)�B'�B+�B*�B+�B+�B,�B-�B,�B,�B,�B+�B)�B(�B+�B:BK�BN�BP�BQ�BR�BU�BX�BX�BY�B[�B\�B[�B\�B\�B]�B^�BaBb
BcBcBcBdBcBeBh/Bh/Bf"Bh/BlGBkABkABlGBkABmMBoZBp`BqfBtxBuBw�By�Bx�By�B{�B�B��B��B��B��B��B��B��B��B��B�B�B�#B�0B�HB�UB�[B�[B�aB�gB��B��B��B��B��B��B��B��B��B��B��B��B��B��B� B�B�7B�=B�CB�IB�PB�bB�gB�mB�mB�sBϘBҫB��B��B��B��B�B�B�=B�\B�hB�tB��B��B��B��B��B��B	�B	�B	�B	�B	
�B	B	B	7B	hB	"�B	$�B	&�B	&�B	'�B	*�B	.�B	2�B	3�B	7B	:B	<B	>+B	A=B	CJB	EVB	InB	K{B	M�B	P�B	S�B	V�B	W�B	X�B	Y�B	[�B	\�B	^�B	`�B	`�B	cB	gB	mCB	rbB	uuB	v{B	z�B	�B	��B	�B	�B	��B	��B	��B	��B	�B	�B	�%B	�=B	�bB	�nB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�+B	�+B	�7B	�DB	�PB	�VB	�VB	�VB	�VB	�\B	�\B	�bB	�bB	�uB	�{B	̀B	ΆB	όB	όB	όB	ВB	ВB	љB	љB	љB	ҟB	ҟB	ҟB	ҟB	ҟB	ӥB	ҟB	ҟB	ձB	ձB	ձB	ձB	ձB	ַB	ַB	ַB	׽B	׽B	׽B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B	��B
3B
�B
�B
"�B
*�B
.�B
7�B
=�B
E�B
LAB
P�B
T
B
Y�B
]�B
b�B
h5B
l�B
r#B
vp111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.44 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144172022020411441720220204114417  AO  ARCAADJP                                                                    20200618141354    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200618141354  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200618141354  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114417  IP                  G�O�G�O�G�O�                