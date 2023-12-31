CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-11-26T01:00:59Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Ux   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]d   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _`   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gL   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  iH   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q4   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �L   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �P   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �T   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �X   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �\   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20171126010059  20190604095309  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055                            2C  D   APEX                            5374                            041511                          846 @�88j �X1   @�89)�@:��S����cF��n�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�  A   A   A>ffA`  A�  A�33A�33A�  A�  A�  A�  A���B ffBffB  B��B   B(ffB0ffB8  B@  BH  BP  BX  B`  BhffBp  Bw��B�  B�  B�  B�  B���B�  B�33B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�  C   C�C  C  C  C
  C  C  C  C�C  C  C  C  C  C  C   C"  C#�fC&  C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF  CH  CJ  CL�CN  CO�fCR  CT�CV  CX  CZ  C\  C]�fC`  Cb�Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr�Ct  Cv  Cx�Cz  C{�fC~  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C��3C��3C�  C��C�  C��3C�  C��C��C��C��C��C�  C�  C��3C�  C��C��C�  C�  C��C��C��C�  C��3C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��3C��3C�  D   D � D  D� D  D�fDfD�fDfD� D��D� DfD� D  D�fD  D� D	fD	� D	��D
� D  Dy�D  D� D��D� DfD�fD  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D��Dy�D��D� D  Dy�D  D� D  D�fD  D� D   D y�D!  D!�fD"  D"� D#fD#� D#��D$� D%  D%�fD&fD&� D&��D'y�D(  D(� D)fD)� D*  D*�fD+fD+� D,  D,�fD-fD-� D-��D.� D/  D/� D0  D0� D1  D1� D2  D2� D3fD3�fD4  D4� D5fD5�fD6  D6y�D6��D7y�D8  D8�fD9fD9�fD:fD:�fD;  D;� D<  D<� D=fD=�fD>fD>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DDy�DE  DE�fDF  DFy�DG  DG� DH  DHy�DI  DI� DJ  DJ� DK  DK� DK��DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DS��DT� DU  DU� DV  DVy�DW  DW�fDX  DX� DY  DY� DZ  DZy�D[  D[� D\  D\� D\��D]� D^  D^� D_fD_� D_��D`� Da  Da� Db  Db� DcfDc�fDd  Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dh� DifDi� Dj  Dj� Dk  Dk� Dl  Dl�fDm  Dm� Dn  Dn�fDo  Do� Dp  Dp�fDq  Dq� Dr  Dr� Dr��DsY�DxO\D�{�D�U�D� RD��D��3D�33D���D���D�3�D�Z�D��\D���D�0�D�l�Dڣ3D��=D�.D�mD��D��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  @���AffA&ffAD��AfffA�33A�ffA�ffA�33A�33A�33A�33A�  B  B
  B��B34B!��B*  B2  B9��BA��BI��BQ��BY��Ba��Bj  Bq��By34B���B���B���B���B���B���B�  B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B�  B���C ffC� CffCffCffC
ffCffCffCffC� CffCffCffCffCffCffC ffC"ffC$L�C&ffC(� C*ffC,ffC.ffC0ffC2ffC4ffC6ffC8ffC:ffC<ffC>ffC@ffCBffCD� CFffCHffCJffCL� CNffCPL�CRffCT� CVffCXffCZffC\ffC^L�C`ffCb� CdffCfffCh� CjffClffCnffCpffCr� CtffCvffCx� CzffC|L�C~ffC�33C�@ C�@ C�33C�33C�33C�33C�33C�33C�33C�@ C�33C�33C�33C�33C�33C�@ C�33C�33C�@ C�33C�33C�33C�33C�@ C�33C�33C�@ C�33C�33C�33C�&fC�33C�33C�&fC�33C�33C�33C�33C�33C�&fC�33C�33C�33C�33C�33C�33C�@ C�@ C�33C�33C�@ C�33C�33C�&fC�33C�33C�33C�33C�33C�33C�@ C�33C�33C�33C�33C�&fC�33C�33C�33C�&fC�&fC�33C�@ C�33C�&fC�33C�@ C�@ C�@ C�@ C�@ C�33C�33C�&fC�33C�@ C�@ C�33C�33C�@ C�@ C�@ C�33C�&fC�&fC�&fC�&fC�&fC�33C�33C�33C�33C�33C�33C�33C�@ C�@ C�@ C�@ C�@ C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�&fC�&fC�33C�&fC�&fC�33D �D ��D�D��D�D� D  D� D  D��D4D��D  D��D�D� D�D��D	  D	��D
4D
��D�D�4D�D��D4D��D  D� D�D��D4D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D  D��D�D��D4D�4D4D��D�D�4D�D��D�D� D�D��D �D �4D!�D!� D"�D"��D#  D#��D$4D$��D%�D%� D&  D&��D'4D'�4D(�D(��D)  D)��D*�D*� D+  D+��D,�D,� D-  D-��D.4D.��D/�D/��D0�D0��D1�D1��D2�D2��D3  D3� D4�D4��D5  D5� D6�D6�4D74D7�4D8�D8� D9  D9� D:  D:� D;�D;��D<�D<��D=  D=� D>  D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD�4DE�DE� DF�DF�4DG�DG��DH�DH�4DI�DI��DJ�DJ��DK�DK��DL4DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT4DT��DU�DU��DV�DV�4DW�DW� DX�DX��DY�DY��DZ�DZ�4D[�D[��D\�D\��D]4D]��D^�D^��D_  D_��D`4D`��Da�Da��Db�Db��Dc  Dc� Dd�Dd�4De4De�4Df4Df�4Dg4Dg�4Dh4Dh��Di  Di��Dj�Dj��Dk�Dk��Dl�Dl� Dm�Dm��Dn�Dn� Do�Do��Dp�Dp� Dq�Dq��Dr�Dr��Ds4Dss4Dxh�D���D�b�D�-D�&�D� D�@ D��gD��RD�@RD�g\D��)D��D�=�D�y�Dڰ D��
D�:�D�y�D�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�O�A�ZA�XA�XA�XA�\)A�bNA�bNA�bNA�bNA�dZA�dZA�bNA�^5A�bNA�dZA�ffA�ffA�ffA�ffA�hsA�jA�jA�l�A�l�A�l�A�l�A�p�A�n�A�p�A�p�A�r�A�jA�O�A�M�A�M�A�K�A�/A��#A�bA�E�A���A�oA�  A��A��;A�|�A�JA�(�A���A�x�A�%A�A��A�%A�S�A�ƨA���A��PA�M�A�{A��HA���A���A�^5A���A�|�A��RA�`BA��DA��RA��TA��A�ffA��\A�A��mA��9A��yA�n�A��A���A�JA�hsA�JA�bA��HA�jA�
=A���A�`BA��A���A�1'A���A�A��uA��yA��A�$�A�ƨA�ffA��uA�n�A���A�l�A~(�A|A�AzbAx~�Av�`Au�^At�uAs�Ar�Aq�FAp$�Am�AlJAj�Ai�Ag��Ae��Ad(�AbbNAat�A_t�A]`BAW�FAV~�AQ�AP��AP��AP��AOAM�;AN�RAL��AH�+AG�AGC�AF^5AD��AC�ACoAB��AB9XAA��AA�A?ƨA>��A=�PA;O�A:��A:�A:A�A9��A8bNA7�A6=qA4�uA2��A1�-A1dZA0��A/��A/�A.��A-33A,5?A+hsA+/A+%A*�9A(�A(n�A(�A'l�A&jA%7LA$�!A$�DA#hsA"1A!/A �A bAS�A�HA�AC�A��A��A�9A��A�A��AVA9XA�AG�A�RAQ�A�7A"�A��A�A�A�wA��A�PAS�A��A �A��AVA�uA�FA��A��A\)AA
�!A
-A	�^A	+A�AI�A�A�AVA�A  A��A1'Ap�AjA`BA 1'@�p�@�{@�V@�j@�!@�7@�9@�(�@@�o@��-@�1@��@��H@��@��`@��m@���@�hs@�u@���@�o@��@�v�@�$�@��@�t�@�E�@���@��@�G�@ؼj@��;@�\)@�5?@Դ9@�n�@�Q�@���@�X@�1'@˅@�~�@�$�@Ɂ@ȣ�@�(�@�K�@�I�@�{@��j@�  @�M�@�bN@��+@���@���@�1'@���@��9@�j@�b@��R@��@��@��#@���@���@�A�@��F@�"�@�ff@�hs@��@�9X@��@���@�\)@�o@��@���@�V@���@��@�|�@�M�@���@�?}@��@��F@��R@�M�@�-@��#@���@�1'@��
@���@�l�@�"�@�n�@�7L@�Ĝ@�Q�@�1@���@��@�=q@���@�?}@���@��@��w@��w@���@���@�M�@��h@�%@�Ĝ@���@�r�@�(�@���@���@��F@�|�@��@��!@�n�@�5?@��@�{@���@���@���@�O�@���@�I�@�  @���@���@�\)@�+@��y@���@�M�@�/@��;@�C�@��@���@��+@�v�@�^5@�5?@�J@��#@�x�@�/@���@�r�@��@�|�@��@��R@�n�@�^5@��-@�O�@�?}@�/@��/@��u@�(�@���@�;d@��@��!@�~�@�=q@�@��@���@��7@�G�@�%@��9@��@�I�@�b@�w@\)@~��@~V@}�h@|��@|�@|I�@{ƨ@{��@{�
@{ƨ@{ƨ@{��@|1@{�m@{�m@|Z@|Z@|Z@|(�@{�F@z��@y�@y��@yhs@y7L@y7L@y7L@y�@x�9@x��@xĜ@xĜ@xA�@w�@w��@wK�@v�y@vv�@v{@up�@t�j@tz�@s��@s�
@s�F@s��@s"�@s@r�@r�@r�@r��@r��@r�\@o�
@h�@a7L@Z.�@TN�@RO@M�@Gt�@B�r@:�]@3�@-��@'U�@"-@�9@~@u�@*0@x@@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A�O�A�ZA�XA�XA�XA�\)A�bNA�bNA�bNA�bNA�dZA�dZA�bNA�^5A�bNA�dZA�ffA�ffA�ffA�ffA�hsA�jA�jA�l�A�l�A�l�A�l�A�p�A�n�A�p�A�p�A�r�A�jA�O�A�M�A�M�A�K�A�/A��#A�bA�E�A���A�oA�  A��A��;A�|�A�JA�(�A���A�x�A�%A�A��A�%A�S�A�ƨA���A��PA�M�A�{A��HA���A���A�^5A���A�|�A��RA�`BA��DA��RA��TA��A�ffA��\A�A��mA��9A��yA�n�A��A���A�JA�hsA�JA�bA��HA�jA�
=A���A�`BA��A���A�1'A���A�A��uA��yA��A�$�A�ƨA�ffA��uA�n�A���A�l�A~(�A|A�AzbAx~�Av�`Au�^At�uAs�Ar�Aq�FAp$�Am�AlJAj�Ai�Ag��Ae��Ad(�AbbNAat�A_t�A]`BAW�FAV~�AQ�AP��AP��AP��AOAM�;AN�RAL��AH�+AG�AGC�AF^5AD��AC�ACoAB��AB9XAA��AA�A?ƨA>��A=�PA;O�A:��A:�A:A�A9��A8bNA7�A6=qA4�uA2��A1�-A1dZA0��A/��A/�A.��A-33A,5?A+hsA+/A+%A*�9A(�A(n�A(�A'l�A&jA%7LA$�!A$�DA#hsA"1A!/A �A bAS�A�HA�AC�A��A��A�9A��A�A��AVA9XA�AG�A�RAQ�A�7A"�A��A�A�A�wA��A�PAS�A��A �A��AVA�uA�FA��A��A\)AA
�!A
-A	�^A	+A�AI�A�A�AVA�A  A��A1'Ap�AjA`BA 1'@�p�@�{@�V@�j@�!@�7@�9@�(�@@�o@��-@�1@��@��H@��@��`@��m@���@�hs@�u@���@�o@��@�v�@�$�@��@�t�@�E�@���@��@�G�@ؼj@��;@�\)@�5?@Դ9@�n�@�Q�@���@�X@�1'@˅@�~�@�$�@Ɂ@ȣ�@�(�@�K�@�I�@�{@��j@�  @�M�@�bN@��+@���@���@�1'@���@��9@�j@�b@��R@��@��@��#@���@���@�A�@��F@�"�@�ff@�hs@��@�9X@��@���@�\)@�o@��@���@�V@���@��@�|�@�M�@���@�?}@��@��F@��R@�M�@�-@��#@���@�1'@��
@���@�l�@�"�@�n�@�7L@�Ĝ@�Q�@�1@���@��@�=q@���@�?}@���@��@��w@��w@���@���@�M�@��h@�%@�Ĝ@���@�r�@�(�@���@���@��F@�|�@��@��!@�n�@�5?@��@�{@���@���@���@�O�@���@�I�@�  @���@���@�\)@�+@��y@���@�M�@�/@��;@�C�@��@���@��+@�v�@�^5@�5?@�J@��#@�x�@�/@���@�r�@��@�|�@��@��R@�n�@�^5@��-@�O�@�?}@�/@��/@��u@�(�@���@�;d@��@��!@�~�@�=q@�@��@���@��7@�G�@�%@��9@��@�I�@�b@�w@\)@~��@~V@}�h@|��@|�@|I�@{ƨ@{��@{�
@{ƨ@{ƨ@{��@|1@{�m@{�m@|Z@|Z@|Z@|(�@{�F@z��@y�@y��@yhs@y7L@y7L@y7L@y�@x�9@x��@xĜ@xĜ@xA�@w�@w��@wK�@v�y@vv�@v{@up�@t�j@tz�@s��@s�
@s�F@s��@s"�@s@r�@r�@r�@r��@r��G�O�@o�
@h�@a7L@Z.�@TN�@RO@M�@Gt�@B�r@:�]@3�@-��@'U�@"-@�9@~@u�@*0@x@@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�sB�NB�BȴB�dB�-B��B��B�uB��B��B��B�\B�B{�B^5BF�B49B49B-B+B+B,B�B�B�B\B
=B��B�B�`B�ZB�NB�#B��BȴB��B�FB�B��B�PB�B{�Bv�Bo�BffB]/BT�BI�B<jB5?B2-B/B0!B@�B?}B\B
�NB
�
B
��B
��B
ƨB
ƨB
ĜB
��B
�RB
��B
��B
��B
�%B
w�B
iyB
ZB
O�B
C�B
=qB
5?B
+B
�B
JB	��B	�B	�B	�BB	��B	�LB	��B	�=B	w�B	\)B	E�B	hB��B��B�B�TB�ZB�BB�5B��B�B�
B��B��BƨB��B�}BBȴBǮBȴBŢB�wB�LB�3B�B�B�!B�B�B��B��B��B�uB�VB�\B�uB�{B�uB�hB�hB�VB�=B�7B�7B�1B�%B�%B�+B�+B�+B�B~�B{�By�Bv�Bv�Bt�Br�Bq�Bp�Bn�Bm�Bk�BffBbNB[#BQ�BK�BJ�BL�BP�BO�BW
BVBT�BVBVBT�BT�BVBVBVBVBVBW
BW
BYBYBYBYBYBZB[#B[#B\)B\)B\)B_;B_;B\)B\)BZBYBXBW
BVBT�BT�BS�BS�BP�BI�B?}B;dB1'B,B,B+B+B+B)�B)�B+B+B)�B+B,B,B,B/B1'B33B6FB7LB;dB@�B@�BA�BA�BC�BE�BD�BB�B@�B>wB<jB:^B:^B9XB7LB6FB6FB6FB7LB9XB:^B:^B8RB2-B)�B%�B%�B%�B"�B �B �B!�B!�B!�B!�B$�B%�B'�B+B1'B33B5?B9XB=qBB�BC�BF�BH�BM�BO�BQ�BQ�BR�BR�BR�BW
BXBZBZBZB\)B^5B_;B_;B`BBbNBffBgmBgmBhsBl�Bo�Bp�Bq�Br�Br�Bu�By�Bz�B{�B{�B|�B�B�B�7B�=B�JB�VB�uB�uB�uB��B��B��B��B��B��B��B�B�B�B�B�B�'B�?B�LB�XB�XB�XB�XB�^B�^B�jB��BÖBŢBƨBȴB��B��B��B��B��B��B�
B�B�#B�/B�5B�5B�;B�;B�BB�HB�ZB�`B�mB�yB�B�B�B��B��B��B��B	B	B	B	B	%B		7B	PB	hB	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	&�B	)�B	-B	0!B	33B	6FB	9XB	<jB	?}B	C�B	I�B	L�B	O�B	Q�B	S�B	YB	[#B	\)B	_;B	aHB	aHB	bNB	ffB	gmB	hsB	iyB	k�B	l�B	m�B	n�B	p�B	r�B	s�B	u�B	x�B	|�B	}�B	~�B	�B	�B	�+B	�7B	�=B	�JB	�VB	�bB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�KB	��B	��B	��B
B
CB
%zB
/OB
5�B
9	B
?HB
F�B
J�B
S�B
\]B
c�B
h�B
l�B
sB
xB
|111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B�aB�]B�[B�[B�[B�VB�[B�]B�[B�]B�]B�cB�cB�iB�cB�cB�cB�[B�]B�[B�[B�]B�[B�[B�[B�cB�cB�cB�[B�]B�]B�]B�]B�lB�oB�iB�ZB�QB�.B��BȐB�DB�B��B��B�SB�dB�xB�^B�9B��B{�B^BF�B4B4B,�B*�B*�B+�B�B{B^B9B
B��B�`B�:B�3B�(B��B��BȍB�]B�!B��B��B�)B��B{�Bv�BoyBf=B]
BT�BI�B<CB5B2B.�B/�B@\B?VB3B
�#B
��B
оB
ʘB
�B
ƀB
�vB
�_B
�+B
��B
��B
�fB
��B
w�B
iOB
Y�B
O�B
CmB
=HB
5B
*�B
�B
!B	��B	�mB	�\B	�B	˜B	�#B	��B	�B	w�B	[�B	EwB	<B��BήB��B�)B�0B�B�	B��B�B��BίBʓB�|B�]B�OB�bBȇBǀBȊB�uB�JB�B�B��B��B��B��B��B��B��B�oB�KB�*B�.B�GB�MB�IB�<B�:B�)B�B�
B�
B�B��B��B� B��B��B��B~�B{�By�Bv�Bv�Bt�Br�Bq}BpuBnlBmbBkTBf7BbBZ�BQ�BK�BJ�BL�BP�BO�BV�BU�BT�BU�BU�BT�BT�BU�BU�BU�BU�BU�BV�BV�BX�BX�BX�BX�BX�BY�BZ�BZ�B[�B[�B[�B_
B_
B[�B[�BY�BX�BW�BV�BU�BT�BT�BS�BS�BP�BI�B?NB;2B0�B+�B+�B*�B*�B*�B)�B)�B*�B*�B)�B*�B+�B+�B+�B.�B0�B3B6B7B;2B@SB@TBAWBAXBCeBEpBDkBB\B@RB>CB<8B:+B:,B9%B7B6B6B6B7B9$B:-B:-B8 B1�B)�B%�B%�B%�B"�B �B �B!�B!�B!�B!�B$�B%�B'�B*�B0�B2�B5B9%B=<BB[BCcBFtBHBM�BO�BQ�BQ�BR�BR�BR�BV�BW�BY�BY�BY�B[�B^ B_B_	B`BbBf3Bg8Bg9Bh?BlUBojBpqBquBr{Br|Bu�By�Bz�B{�B{�B|�B��B��B�B�B�B�$B�?B�?B�?B�RB�sB��B��B��B��B��B��B��B��B��B��B��B�
B�B�%B�#B�#B�#B�)B�,B�3B�OB�`B�nB�sB�BʈBʌB̗B˒B͜BѸB��B��B��B��B��B�B�B�B�B�B�&B�*B�6B�CB�VB�qB�~B��B��B��B��B	 �B	�B	 �B	�B	�B		 B	B	5B	MB	TB	]B	qB	~B	�B	�B	!�B	#�B	&�B	)�B	,�B	/�B	2�B	6B	9"B	<4B	?GB	C`B	I�B	L�B	O�B	Q�B	S�B	X�B	Z�B	[�B	_B	aB	aB	bB	f1B	g9B	h>B	iDB	kNB	lWB	mXB	nfB	pnB	r{B	s�B	u�B	x�B	|�B	}�B	~�B	��B	��B	��B	�B	�	B	�B	� B	�,B	�:B	�@B	�FB	�LB	�TB	�WB	�XB	�iB	�uB	�xB	�}B	�}B	��B	��G�O�B	�B	ǖB	ޟB	�pB
�B
B
%GB
/B
5XB
8�B
?B
F�B
J�B
S�B
\)B
coB
hXB
l�B
r�B
w�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.4 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201906040953092019060409530920190604095309  AO  ARCAADJP                                                                    20171126010059    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20171126010059  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20171126010059  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604095309  IP                  G�O�G�O�G�O�                