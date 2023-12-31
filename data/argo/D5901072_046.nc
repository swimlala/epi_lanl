CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:52Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �\   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �`   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �h   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �l   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               .A   AO  20111130143949  20190522121829  1728_5048_046                   2C  D   APEX                            2142                            040306                          846 @ԝ#ZC��1   @ԝ#�s��@6Rn��O��c'�O�;d1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @y��@�  A   A   A@  A`  A���A�  A�  A���A���A���A�  A�  B   B  B��B��B   B(  B0ffB8  B@  BH  BP  BX  B`  Bg��Bp  BxffB�33B�  B���B���B�  B�  B�  B�  B�33B�33B�33B�  B���B�  B�  B�  B�  B���B�  B�33B�33B�  B���B�  B�33B�  B�  B�  B���B�  B�  B�  C   C  C�C�C  C	�fC�fC�fC�fC  C  C  C�fC  C  C  C   C"  C$�C&  C(  C*�C,  C-�fC0  C2  C3�fC5�fC8  C:  C<  C>�C@  CA�fCD  CF�CH  CI�fCL  CN�CP  CQ�fCT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct�Cv  Cw�fCz  C|�C~�C�  C�  C��3C�  C��C��C��C�  C��3C��3C�  C��C��C��C��C��C��C�  C��3C��3C��3C��3C�  C��C�  C�  C��3C�  C��C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C��C�  C��C��C�  C�  C��3C�  C��C��C��C��C��C��C��C��C��C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C��3C�  C��C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C��C��C�  C��3C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C��C�  C��3C�  C��C�  C��3C�  C�  D   D �fD  D� D  D� D  D� DfD� D��D� D  Dy�DfD� D  D� D	  D	� D
  D
�fDfD� D  D� D  D� D  Dy�D  D� D  D� D  D� D��D� D  D� D��Dy�D��Dy�D  D� D  D� DfD� D��Dy�D��D� D  D�fD  Dy�D  D� DfD�fD  D� D fD � D ��D!y�D!��D"y�D#  D#� D$  D$y�D$��D%� D&  D&� D&��D'� D(  D(� D(��D)� D*fD*�fD+  D+� D,  D,y�D-  D-�fD.  D.� D.��D/� D0  D0y�D1  D1� D1��D2� D3  D3� D4  D4� D5  D5� D6fD6� D6��D7� D8  D8� D8��D9� D:  D:� D;  D;�fD<  D<y�D<��D=y�D=��D>y�D?  D?� D@  D@� DAfDA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI� DJ  DJ�fDK  DK� DL  DL�fDM  DM� DN  DN�fDO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU�fDV  DVy�DV��DW� DXfDX� DY  DY� DY��DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_y�D`  D`� Da  Da�fDb  Db� Dc  Dcy�Dd  Dd� De  De� Df  Df� DgfDg� Dh  Dh� Di  Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dm� Dn  Dn� DofDo� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dy�3D��D�)�D�c3D���D���D�33D���D���D���D�3D�y�DǬ�D��3D�)�DچfD�3D��3D�3D�S31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @fff@�ff@�ffA33A;33A[33A|��A���A���A�ffA�ffA�ffAݙ�A홚A���B��BffBffB��B&��B/33B6��B>��BF��BN��BV��B^��BfffBn��Bw33B33B�ffB�33B�33B�ffB�ffB�ffB�ffB���B���B���B�ffB�33B�ffB�ffB�ffB�ffB�33B�ffB˙�Bϙ�B�ffB�33B�ffBߙ�B�ffB�ffB�ffB�33B�ffB�ffB�ffB�ffC�3C��C��C�3C	��C��C��C��C�3C�3C�3C��C�3C�3C�3C�3C!�3C#��C%�3C'�3C)��C+�3C-��C/�3C1�3C3��C5��C7�3C9�3C;�3C=��C?�3CA��CC�3CE��CG�3CI��CK�3CM��CO�3CQ��CS�3CU��CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm��Co�3Cq�3Cs��Cu�3Cw��Cy�3C{��C}��C�3C�ٚC���C�ٚC��fC��fC��fC�ٚC���C���C�ٚC��fC��fC��fC��fC��fC��fC�ٚC���C���C���C���C�ٚC��fC�ٚC�ٚC���C�ٚC��fC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC���C�ٚC��fC�ٚC��fC��fC�ٚC�ٚC���C�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC���C�ٚC��fC�ٚC���C�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C���C�ٚC��fC��fC��fC�ٚC���C�ٚC��fC�ٚC�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC���C�ٚC��fC�ٚC���C�ٚC�ٚC�ٚD s3D ��Dl�D��Dl�D��Dl�D�3Dl�D�fDl�D��DffD�3Dl�D��Dl�D��D	l�D	��D
s3D
�3Dl�D��Dl�D��Dl�D��DffD��Dl�D��Dl�D��Dl�D�fDl�D��Dl�D�fDffD�fDffD��Dl�D��Dl�D�3Dl�D�fDffD�fDl�D��Ds3D��DffD��Dl�D�3Ds3D��Dl�D�3D l�D �fD!ffD!�fD"ffD"��D#l�D#��D$ffD$�fD%l�D%��D&l�D&�fD'l�D'��D(l�D(�fD)l�D)�3D*s3D*��D+l�D+��D,ffD,��D-s3D-��D.l�D.�fD/l�D/��D0ffD0��D1l�D1�fD2l�D2��D3l�D3��D4l�D4��D5l�D5�3D6l�D6�fD7l�D7��D8l�D8�fD9l�D9��D:l�D:��D;s3D;��D<ffD<�fD=ffD=�fD>ffD>��D?l�D?��D@l�D@�3DAl�DA��DBl�DB��DCl�DC��DDl�DD��DEl�DE��DFl�DF��DGl�DG��DHl�DH�3DIl�DI��DJs3DJ��DKl�DK��DLs3DL��DMl�DM��DNs3DN��DOl�DO��DPl�DP��DQl�DQ��DRl�DR��DSl�DS��DTl�DT��DUs3DU��DVffDV�fDWl�DW�3DXl�DX��DYl�DY�fDZl�DZ��D[l�D[��D\l�D\��D]l�D]��D^l�D^��D_ffD_��D`l�D`��Das3Da��Dbl�Db��DcffDc��Ddl�Dd��Del�De��Dfl�Df�3Dgl�Dg��Dhl�Dh��DiffDi�fDjffDj�fDkffDk�fDlffDl�fDml�Dm��Dnl�Dn�3Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dsl�Ds��Dtl�Dt��Dul�Du��Dvl�Dv��Dwl�Dy� D�3D�  D�Y�D�� D�� D�)�D��3D�� D��3D�	�D�p Dǣ3D��D�  D�|�Dਗ਼D�ɚD���D�I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�{A�"�A��A�(�A�(�A�+A�+A�(�A�+A�+A�-A�-A�-A�/A�/A�1'A�33A�5?A�5?A�33A�5?A�33A�1'A�+A�VA��A��TA���A˗�A�v�A�ffA�`BA�^5A�^5A�VA�E�A�33A�$�A�%A���A��/A�|�A�hsA�+A�t�A�%A��;A��#A���A���A�%A���A�A�K�A��A��hA�?}A���A���A�ƨA�
=A�1'A��wA�&�A��wA�dZA��mA��A��7A���A���A���A�1'A���A�VA�$�A�ƨA�1'A�E�A���A��A�^5A��A�t�A��A��+A�n�A��7A���A�1A�%A��A�1A�Q�A���A��yA�&�A���A�I�A�n�A�K�A�G�A���A���A�/A���A�/A�I�A�dZA�n�A�bA�G�A�A�bNA�C�A���A��A��HA��A�^5A�&�A�A~��A|bAzAz  AyC�Ax$�Av�`At�Ar�DAox�AmXAk��Ai"�Afr�Ad��Ab9XA^�RA\�!A[33AZA�AY�AX{AV�HAT^5AS&�ARJAPȴAN��AJr�AG�mAFbNAE��AD�AC�AAXA?\)A=�A;O�A9+A7�wA5x�A3��A2�A1�PA0{A.ZA,��A+�PA+�A*v�A)��A)��A)|�A(�/A'��A'x�A&�`A&A�A%��A%�A$�+A#�A#"�A!�;A 1'A��A��AVAp�AȴA�wA�#A��A  A��A�^A��A�7AO�A�`Ar�A$�A��Ar�AK�A�jA �A�wAp�A�A~�A�A1AVAA�A+A
1A	�A|�An�AƨAA{A`BAA�A �H@��@��-@���@�j@�"�@��@��9@�@���@�
=@��T@�A�@�@�
=@�^5@�@�9@�ƨ@���@�$�@�^@�9@�dZ@�{@�%@�A�@��;@�\)@݁@���@�C�@��y@ڇ+@�E�@ى7@؃@��@�^5@ղ-@�9X@҇+@Ѻ^@���@�  @�;d@�%@��m@˝�@�
=@��T@�`B@�7L@�%@�z�@�  @�l�@ư!@��@�X@�Ĝ@��;@�S�@��@��H@¸R@§�@�V@���@��@�(�@�t�@���@�@��@��@��@���@�ff@���@�X@��@��@��;@���@���@�t�@�"�@��+@��^@�%@�Z@���@�o@���@�^5@�-@�@�hs@�O�@�/@��@��@��@��@��@��@���@��9@�z�@�1'@���@�33@�ȴ@���@��+@�~�@�v�@�n�@�M�@��@���@��@��7@���@��h@��7@�p�@�?}@���@���@�(�@���@���@��w@���@��@���@�&�@�bN@�A�@��@���@�|�@��@��@�=q@��T@��#@���@�X@�z�@���@���@�G�@�Ĝ@�b@�\)@���@��\@�E�@�V@�E�@���@��h@�G�@�%@��@��/@���@�9X@�1@��m@��w@�ƨ@�  @�1@��m@��y@���@�K�@�33@��y@�^5@�@�O�@�Q�@�+@��@�V@���@���@�V@�r�@�b@�t�@���@�ȴ@��\@�n�@�E�@�{@��^@�`B@���@��j@��@���@���@�%@�p�@��@�p�@�hs@��@�Q�@��@��P@�S�@��H@�v�@���@��#@���@���@��@��@��@�bN@�I�@�9X@�A�@�(�@�  @��;@���@���@�dZ@�"�@��y@��R@���@�n�@�E�@�$�@��@��h@�?}@��@�V@��@���@���@�A�@� �@�b@�1@�  @���@��m@��;@�ƨ@��F@�S�@���@��H@���@���@�7L@xĜ@p��@ix�@bM�@[��@So@L�@E��@?
=@8r�@333@-��@(�@"~�@�/@�`@��@\)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�{A�"�A��A�(�A�(�A�+A�+A�(�A�+A�+A�-A�-A�-A�/A�/A�1'A�33A�5?A�5?A�33A�5?A�33A�1'A�+A�VA��A��TA���A˗�A�v�A�ffA�`BA�^5A�^5A�VA�E�A�33A�$�A�%A���A��/A�|�A�hsA�+A�t�A�%A��;A��#A���A���A�%A���A�A�K�A��A��hA�?}A���A���A�ƨA�
=A�1'A��wA�&�A��wA�dZA��mA��A��7A���A���A���A�1'A���A�VA�$�A�ƨA�1'A�E�A���A��A�^5A��A�t�A��A��+A�n�A��7A���A�1A�%A��A�1A�Q�A���A��yA�&�A���A�I�A�n�A�K�A�G�A���A���A�/A���A�/A�I�A�dZA�n�A�bA�G�A�A�bNA�C�A���A��A��HA��A�^5A�&�A�A~��A|bAzAz  AyC�Ax$�Av�`At�Ar�DAox�AmXAk��Ai"�Afr�Ad��Ab9XA^�RA\�!A[33AZA�AY�AX{AV�HAT^5AS&�ARJAPȴAN��AJr�AG�mAFbNAE��AD�AC�AAXA?\)A=�A;O�A9+A7�wA5x�A3��A2�A1�PA0{A.ZA,��A+�PA+�A*v�A)��A)��A)|�A(�/A'��A'x�A&�`A&A�A%��A%�A$�+A#�A#"�A!�;A 1'A��A��AVAp�AȴA�wA�#A��A  A��A�^A��A�7AO�A�`Ar�A$�A��Ar�AK�A�jA �A�wAp�A�A~�A�A1AVAA�A+A
1A	�A|�An�AƨAA{A`BAA�A �H@��@��-@���@�j@�"�@��@��9@�@���@�
=@��T@�A�@�@�
=@�^5@�@�9@�ƨ@���@�$�@�^@�9@�dZ@�{@�%@�A�@��;@�\)@݁@���@�C�@��y@ڇ+@�E�@ى7@؃@��@�^5@ղ-@�9X@҇+@Ѻ^@���@�  @�;d@�%@��m@˝�@�
=@��T@�`B@�7L@�%@�z�@�  @�l�@ư!@��@�X@�Ĝ@��;@�S�@��@��H@¸R@§�@�V@���@��@�(�@�t�@���@�@��@��@��@���@�ff@���@�X@��@��@��;@���@���@�t�@�"�@��+@��^@�%@�Z@���@�o@���@�^5@�-@�@�hs@�O�@�/@��@��@��@��@��@��@���@��9@�z�@�1'@���@�33@�ȴ@���@��+@�~�@�v�@�n�@�M�@��@���@��@��7@���@��h@��7@�p�@�?}@���@���@�(�@���@���@��w@���@��@���@�&�@�bN@�A�@��@���@�|�@��@��@�=q@��T@��#@���@�X@�z�@���@���@�G�@�Ĝ@�b@�\)@���@��\@�E�@�V@�E�@���@��h@�G�@�%@��@��/@���@�9X@�1@��m@��w@�ƨ@�  @�1@��m@��y@���@�K�@�33@��y@�^5@�@�O�@�Q�@�+@��@�V@���@���@�V@�r�@�b@�t�@���@�ȴ@��\@�n�@�E�@�{@��^@�`B@���@��j@��@���@���@�%@�p�@��@�p�@�hs@��@�Q�@��@��P@�S�@��H@�v�@���@��#@���@���@��@��@��@�bN@�I�@�9X@�A�@�(�@�  @��;@���@���@�dZ@�"�@��y@��R@���@�n�@�E�@�$�@��@��h@�?}@��@�V@��@���@���@�A�@� �@�b@�1@�  @���@��m@��;@�ƨ@��F@�S�@���@��H@���@���@�7L@xĜ@p��@ix�@bM�@[��@So@L�@E��@?
=@8r�@333@-��@(�@"~�@�/@�`@��@\)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�PB�JB�PB�VB�\B�VB�PB�PB�VB�PB�DB�7B�B�B{�B}�B�B�DB��B��B�XB�FB�?B��B��B�#B�#B�
B��B��B��B��B��BȴBƨBŢBÖBB�}B�jB�qB�^B�LB�?B�?B�?B�9B�'B��B��B��B�{B�bB�JB�7B�B{�Be`B_;B\)BN�B=qB,B#�B�BbBB��B�B�B�NB��B��B�FB��B��B�BS�B0!B�B%B
��B
�B
��B
�9B
��B
��B
�DB
�B
s�B
W
B
:^B
0!B
%�B
{B
1B
�B
�B
�B
VB
B	��B	�B	�fB	�5B	��B	ĜB	�LB	�B	��B	�+B	|�B	r�B	gmB	]/B	XB	F�B	@�B	;dB	)�B	uB�B��B��B�#B�/B��BŢB�wB�3B��B��B�JB{�Bs�Bn�BgmBgmBk�Bk�Bm�Br�Bu�Bv�Bz�B{�B~�B� B�1B�DB�PB�VB�VB�VB�VB�DB�7B�7B�DB�DB�JB�bB�bB�hB�hB�VB�VB�\B�\B�\B�bB�bB�bB�VB�DB�1B�B~�B{�B|�B{�B{�Bz�Bx�Bt�Bn�Bn�Bl�BiyBffBbNB_;B]/B[#B\)BYBYBVBP�BM�BL�BJ�BH�BH�BI�BN�BP�BQ�BQ�BR�BT�BS�BT�BS�BS�BVBVBW
BW
BVBVBVBW
BVBT�BT�BS�BW
BYBXBXBXBXBXBXB[#BZBYB\)B`BB`BBbNBbNBcTBm�Bo�Bp�Bq�Bv�Bx�By�Bz�B}�B�B�B�1B�=B�\B�oB��B��B��B��B��B��B��B��B��B�B�9B��BŢBɺB��B��B��B��B��B�B�
B�
B�B�B�)B�;B�BB�ZB�mB�B�B��B	B	
=B	JB	VB	hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	)�B	.B	33B	9XB	>wB	C�B	F�B	G�B	H�B	H�B	H�B	I�B	J�B	L�B	M�B	P�B	VB	XB	YB	\)B	^5B	aHB	bNB	cTB	cTB	dZB	e`B	ffB	e`B	dZB	aHB	aHB	dZB	dZB	e`B	ffB	k�B	m�B	k�B	k�B	k�B	l�B	l�B	k�B	iyB	gmB	gmB	gmB	jB	l�B	m�B	o�B	q�B	t�B	t�B	v�B	v�B	w�B	z�B	{�B	}�B	�B	�B	�%B	�+B	�7B	�DB	�VB	�\B	�bB	�hB	�oB	��B	��B	��B	��B	��B	�{B	�bB	�VB	�VB	�JB	�JB	�JB	�DB	�PB	�VB	�hB	�uB	�{B	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�9B	�?B	�^B	�wB	�wB	�}B	�}B	��B	B	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�)B	�)B	�)B	�/B	�/B	�5B	�;B	�;B	�BB	�BB	�HB	�TB	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
	7B
uB
�B
!�B
(�B
1'B
7LB
?}B
D�B
I�B
Q�B
W
B
ZB
_;B
bNB
jB
m�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�VB�JB�PB�VB�bB�\B�VB�VB�\B�VB�VB�bB��B��B��B��B��B��B��B�B��B�jB�qBɺB�#B�sB�B�mB�NB�/B�#B�B��B��B��B��B��B��BǮBƨBĜB�}B�jB�dB�XB�RB�^B�XB�3B��B��B��B��B�hB�\B�VB�7Bp�BiyBhsB]/BK�B5?B-B$�B�BVBB��B��B�B�/B��BB�FB�?B��BjB=qB&�BbBJB
=B
�HB
B
�B
��B
��B
�oB
�B
dZB
A�B
;dB
6FB
�B
DB
#�B
'�B
#�B
�B
bB
VB
B	��B	�B	�mB	��B	ɺB	ĜB	��B	�hB	�B	{�B	q�B	jB	hsB	P�B	J�B	G�B	;dB	0!B��B�;B�
B�TB�B�5B��B��BŢB�9B��B��B�%B}�By�Bs�Bt�Bv�Br�Bq�Bv�By�Bx�B~�B�B�B�B�PB�bB�oB�uB�uB�{B��B��B�{B�hB�VB�\B�uB��B��B��B��B�oB�\B�bB�bB�hB�uB�{B�{B�oB�hB�hB�=B�B�B� B~�B�B� B�B�Bv�Bv�Bu�Br�Bo�Bm�BgmBcTBbNBcTB`BBcTB`BBZBR�BO�BN�BO�BS�BXBZBW
BVBW
BYBXBW
BXBW
BXBZBZBZBZBZB\)B[#B[#BYBXBYBS�B\)B\)BZBZBZB[#BXB]/B[#B^5B_;B\)B`BBdZBffBgmBjBq�Bq�Bp�Bq�Bx�By�Bz�B|�B� B�B�B�DB�PB�oB��B��B��B��B��B��B��B��B��B��B�B�LBÖBɺB��B��B��B��B��B��B�B�B�
B�B�B�/B�HB�BB�ZB�B�B�B��B	B	
=B	JB	VB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	$�B	+B	.B	5?B	9XB	>wB	D�B	F�B	G�B	H�B	I�B	H�B	J�B	K�B	L�B	M�B	P�B	VB	XB	ZB	\)B	_;B	cTB	dZB	dZB	dZB	dZB	e`B	ffB	iyB	dZB	cTB	bNB	e`B	dZB	e`B	e`B	m�B	q�B	k�B	l�B	k�B	l�B	l�B	k�B	iyB	hsB	iyB	iyB	l�B	l�B	n�B	p�B	q�B	u�B	u�B	w�B	w�B	x�B	{�B	|�B	~�B	�B	�%B	�+B	�1B	�7B	�DB	�VB	�\B	�uB	�hB	�oB	��B	��B	��B	��B	��B	��B	�uB	�\B	�bB	�VB	�PB	�VB	�PB	�\B	�bB	�oB	�{B	��B	�{B	��B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�9B	�?B	�dB	��B	��B	��B	��B	��B	ÖB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�/B	�/B	�/B	�/B	�/B	�5B	�;B	�;B	�BB	�BB	�HB	�ZB	�fB	�fB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
	7B
uB
�B
!�B
(�B
1'B
8RB
?}B
D�B
J�B
Q�B
XB
[#B
_;B
bNB
jB
n�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�C�<�`B=,1<���<���<D��<49X<#�
<#�
<#�
<#�
<#�
<#�
<T��<�o<�o<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<49X<#�
<D��<e`B<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<u<D��<#�
<D��<e`B<�9X<��<�9X<T��<D��<#�
<�C�<�`B<��
<e`B<#�
<D��<u<�C�<�C�<T��<#�
<49X<�o<#�
<#�
<#�
<#�
<D��<�C�<u<��
<�o<u<��
<�t�<u<�t�<�9X<T��<#�
<#�
<#�
<#�
<T��<�o<#�
<#�
<D��<�C�<�`B<�o<#�
<#�
<#�
<T��<e`B<T��<e`B<�t�<e`B<D��<u<#�
<#�
<49X<D��<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452092012011014520920120110145209  AO  ARGQ                                                                        20111130143949  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143949  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145209  IP                  G�O�G�O�G�O�                