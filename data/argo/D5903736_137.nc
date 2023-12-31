CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-01-24T20:16:53Z AOML 3.0 creation; 2016-05-31T19:14:47Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7$   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7(   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7,   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7<   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7L   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7\   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7d   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8    DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8@   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8D   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8H   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8h   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
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
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20160124201653  20190604093959  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051_7090_137                   2C  D   APEX                            5368                            041511                          846 @א[�f*1   @א\|5&�@4�M����dd�9Xb1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C?�fCA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%fD%�fD&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dy� D�	�D�S3D��fD���D� D�VfD���D��fD�fD�I�D�vfD��fD�  D�9�Dڐ D�ɚD���D�0 D�ffD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�\)@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�zCA�zCC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Cp�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D%�D%��D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt[�Dy{�D�]D�P�D��)D��]D��D�T)D��]D��)D�)D�G]D�t)D��)D���D�7]Dڍ�D��]D���D�-�D�d)D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�t�A�x�A�z�A�z�A�x�A�|�Aˇ+A�v�A�9XA���Aʕ�A�VA�dZA�G�A�ĜA�n�A�+A��AƮAƝ�A�A�A��#AŁA��A�ȴA�^5AîA�  A7A�jA�bNA�XA�S�A�O�A�M�A�O�A�VA�^5A�E�A��A��#A���A�ƨA���A��A�O�A�-A�E�A���A��A���A��TA��A�x�A��jA�VA��+A�9XA�^5A�1'A���A�n�A��TA�33A�"�A���A�ĜA�E�A�VA���A��
A��^A�A�A��A��A�bA�`BA�;dA�p�A�{A�?}A�|�A���A�?}A���A�ƨA��hA��A�r�A��/A�A���A���A��\A�`BA��^A��PA���A��jA�  A�M�A��hA��jA�VA���A�jA�~�A��RA���A�&�A��A}�^Azv�Ay��Aw��Au�^At�RAq��An^5AlĜAg��Ad��Ac��Ab�Aax�A^  AYASt�AQ�
AQp�AQ�AO��AO�ANAL��AK�AJ �AIG�AG��AFjAE|�ADQ�AB��AA%A>{A;ƨA:��A9�hA97LA8��A8E�A8bA7�-A7`BA7�A6A�A4��A3�A2�A1l�A0��A/�7A.=qA+�#A)��A(��A'?}A$��A"�A!hsA!
=A �AVA�DA �A�wA��AXAĜA{A��A1A��Ar�AbNA$�A�PA��A�\A�TA?}A�\A33A�DA  A+A~�Ar�AjA9XA��A�wAĜA��A9XAbA��A
z�A	�AAK�A��A�Al�A��Ap�A ��@��P@�^5@�Ĝ@�v�@�/@�\)@�M�@�`B@띲@�\@��@��@�7@�7L@��@�P@�~�@�p�@���@���@��T@܃@�33@�$�@���@ם�@ָR@�V@�
=@�E�@Л�@�J@�O�@�@�&�@��@�\)@ɩ�@�A�@�"�@�~�@��@�@ŉ7@�G�@��@�V@ě�@�1@þw@�l�@�;d@�@\@�n�@�^5@�-@��@�7L@��
@��!@�x�@�1@���@�V@�@��@�@���@�`B@���@�j@���@��F@�|�@�S�@�+@�@��@��H@��R@��+@�V@���@���@�G�@��@�bN@�l�@�ȴ@���@�M�@�J@��@�V@��@��`@��j@�1@��!@�V@��@��#@��7@�?}@���@�Z@� �@��@�|�@�"�@��+@�{@���@���@��w@�l�@�+@��!@�M�@�E�@�E�@�E�@�J@���@���@�x�@�G�@��@���@��@�bN@�(�@���@�\)@�C�@�+@��y@���@��!@���@���@�~�@��@�@�x�@�?}@�7L@��/@��;@��P@��@��H@��\@�V@�{@��@��h@�G�@��@��`@���@��u@�Q�@�1@��m@��w@���@�|�@�K�@�@��y@��!@�E�@���@��-@�`B@�O�@�7L@���@��@��@��
@���@���@���@�;d@�@���@�5?@�^5@�5?@���@��-@���@���@�x�@�/@�V@�%@���@�Q�@�b@��@��P@�S�@���@�^5@���@�5?@��@���@�G�@�Ĝ@�z�@�Q�@�9X@��;@�t�@�K�@���@���@�n�@�=q@�M�@��+@��+@��\@�V@��7@��u@��w@�K�@�dZ@�l�@��!@���@��-@�/@���@���@� �@��;@���@���@��w@�l�@�
=@�
=@�C�@���@��@�@���@��@��T@���@�x�@��@�/@�Ĝ@��/@�X@��7@��@�`B@��@�r�@��@���@���@��@}@st�@kƨ@aX@X�`@O�P@HQ�@B�@<9X@7�P@0��@*n�@$��@�P@��@�T@n�@
=@"�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�t�A�x�A�z�A�z�A�x�A�|�Aˇ+A�v�A�9XA���Aʕ�A�VA�dZA�G�A�ĜA�n�A�+A��AƮAƝ�A�A�A��#AŁA��A�ȴA�^5AîA�  A7A�jA�bNA�XA�S�A�O�A�M�A�O�A�VA�^5A�E�A��A��#A���A�ƨA���A��A�O�A�-A�E�A���A��A���A��TA��A�x�A��jA�VA��+A�9XA�^5A�1'A���A�n�A��TA�33A�"�A���A�ĜA�E�A�VA���A��
A��^A�A�A��A��A�bA�`BA�;dA�p�A�{A�?}A�|�A���A�?}A���A�ƨA��hA��A�r�A��/A�A���A���A��\A�`BA��^A��PA���A��jA�  A�M�A��hA��jA�VA���A�jA�~�A��RA���A�&�A��A}�^Azv�Ay��Aw��Au�^At�RAq��An^5AlĜAg��Ad��Ac��Ab�Aax�A^  AYASt�AQ�
AQp�AQ�AO��AO�ANAL��AK�AJ �AIG�AG��AFjAE|�ADQ�AB��AA%A>{A;ƨA:��A9�hA97LA8��A8E�A8bA7�-A7`BA7�A6A�A4��A3�A2�A1l�A0��A/�7A.=qA+�#A)��A(��A'?}A$��A"�A!hsA!
=A �AVA�DA �A�wA��AXAĜA{A��A1A��Ar�AbNA$�A�PA��A�\A�TA?}A�\A33A�DA  A+A~�Ar�AjA9XA��A�wAĜA��A9XAbA��A
z�A	�AAK�A��A�Al�A��Ap�A ��@��P@�^5@�Ĝ@�v�@�/@�\)@�M�@�`B@띲@�\@��@��@�7@�7L@��@�P@�~�@�p�@���@���@��T@܃@�33@�$�@���@ם�@ָR@�V@�
=@�E�@Л�@�J@�O�@�@�&�@��@�\)@ɩ�@�A�@�"�@�~�@��@�@ŉ7@�G�@��@�V@ě�@�1@þw@�l�@�;d@�@\@�n�@�^5@�-@��@�7L@��
@��!@�x�@�1@���@�V@�@��@�@���@�`B@���@�j@���@��F@�|�@�S�@�+@�@��@��H@��R@��+@�V@���@���@�G�@��@�bN@�l�@�ȴ@���@�M�@�J@��@�V@��@��`@��j@�1@��!@�V@��@��#@��7@�?}@���@�Z@� �@��@�|�@�"�@��+@�{@���@���@��w@�l�@�+@��!@�M�@�E�@�E�@�E�@�J@���@���@�x�@�G�@��@���@��@�bN@�(�@���@�\)@�C�@�+@��y@���@��!@���@���@�~�@��@�@�x�@�?}@�7L@��/@��;@��P@��@��H@��\@�V@�{@��@��h@�G�@��@��`@���@��u@�Q�@�1@��m@��w@���@�|�@�K�@�@��y@��!@�E�@���@��-@�`B@�O�@�7L@���@��@��@��
@���@���@���@�;d@�@���@�5?@�^5@�5?@���@��-@���@���@�x�@�/@�V@�%@���@�Q�@�b@��@��P@�S�@���@�^5@���@�5?@��@���@�G�@�Ĝ@�z�@�Q�@�9X@��;@�t�@�K�@���@���@�n�@�=q@�M�@��+@��+@��\@�V@��7@��u@��w@�K�@�dZ@�l�@��!@���@��-@�/@���@���@� �@��;@���@���@��w@�l�@�
=@�
=@�C�@���@��@�@���@��@��T@���@�x�@��@�/@�Ĝ@��/@�X@��7@��@�`B@��@�r�@��@���@���@��@}@st�@kƨ@aX@X�`@O�P@HQ�@B�@<9X@7�P@0��@*n�@$��@�P@��@�T@n�@
=@"�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB6FB6FB6FB6FB6FB6FB6FB6FB:^BK�B}�B��B��B��B�uB�hB�oB��B��B��B�B�XB��BɺB��B�#B�B��BBBBBBBBBB+B+B\BbBhBbB �B-B33B:^B49B1'B/B33BE�BI�BP�BK�B>wB8RB/B"�B�BPB��B�B�B�yB�mB�sB�yB�yB�mB�`B�BB�/B�B�B��BB�?B�!B�oB�%Bz�Be`BaHBiyBaHBP�BE�B0!B�B1B��B�NB�5B�B��B�wB�-B��B�{By�BiyBXBL�B-B1B
�B
ƨB
�-B
��B
v�B
^5B
F�B
>wB
1'B
%�B
�B
  B	�BB	��B	��B	��B	��B	��B	��B	�B	ffB	G�B	=qB	<jB	9XB	49B	/B	)�B	#�B	�B	�B	{B	PB	%B	  B��B�B�B�;B�B��B��B��B��B��B��B��BɺBǮBĜB��B�wB�jB�^B�XB�LB�9B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B��B��B�B�3B�B��B��B��B��B��B��B��B��B��B�B{�Bv�Bq�BjBm�Bu�Bw�Bx�Bx�Bx�B{�B� B�B�B�B�%B�7B�DB�JB�bB�oB�oB�uB�oB�bB�bB�uB��B��B��B��B��B��B��B��B��B�B�!B�XB�wB��B��BĜBǮBȴBɺB��B��B��B��B��B��B��B��B�B�BB�`B�B�B�B��B��B��B��B��B��B	  B	B	B	B	%B	+B	1B	1B		7B	
=B	DB	JB	\B	hB	{B	�B	�B	�B	�B	�B	$�B	'�B	)�B	,B	.B	0!B	2-B	5?B	=qB	?}B	@�B	B�B	D�B	E�B	H�B	K�B	N�B	Q�B	R�B	T�B	XB	[#B	]/B	_;B	e`B	ffB	hsB	jB	m�B	n�B	n�B	p�B	x�B	z�B	{�B	|�B	}�B	}�B	~�B	�B	�B	�%B	�JB	�PB	�VB	�VB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�3B	�?B	�?B	�RB	�XB	�^B	�dB	�wB	�}B	��B	B	ÖB	ÖB	ĜB	ĜB	ĜB	ŢB	ƨB	ƨB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�)B	�/B	�BB	�BB	�BB	�5B	�5B	�;B	�BB	�NB	�TB	�ZB	�`B	�fB	�`B	�`B	�`B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
+B

=B

=B
�B
�B
+B
2-B
;dB
>wB
B�B
E�B
L�B
R�B
ZB
_;B
cTB
hsB
k�B
o�B
s�B
v�B
y�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B7B7B7B7B7B7B7B7B;)BL�B~�B�zB�rB�RB�@B�7B�@B�PB��B��B��B�#B�TBʇBѷB��B�dB��B�B�B�B�B�B�B�B�B�B�B�B+B)B7B0B!�B-�B3�B;.B5B1�B/�B3�BFvBJ�BQ�BL�B?BB9B/�B#�B`BB��B�bB�MB�JB�9B�EB�EB�DB�6B�-B�B��B��B��BѳB�\B�B��B�9B��B{�Bf)BbBjCBbBQ�BFoB0�BZB�B��B�B�B��BХB�CB��B��B�DBz�BjDBX�BM�B-�B�B
��B
�tB
��B
�KB
w�B
^�B
GsB
?CB
1�B
&�B
zB
 �B	�B	͔B	��B	��B	��B	��B	�oB	��B	g/B	HvB	>:B	=1B	:B	5B	/�B	*�B	$�B	zB	^B	CB	B	�B	 �B��B�B�DB�B��B��BҴBЧBϜB͓B̎BˆBʂB�yB�aB�GB�>B�0B�"B�"B�B� B��B��B��B��B��B��B��B��B�xB�~B�}B��B��B��B�}B�tB�zB��B�wB�~B��B�{B�rB�lB�`B�tB�xB�qB�~B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB��B��B��B��B�NB��B|�Bw�BrqBkFBnTBv�Bx�By�By�By�B|�B��B��B��B��B��B��B�B�B�(B�7B�6B�<B�7B�*B�-B�:B�VB��B��B��B��B��B��B��B��B��B��B�B�>B�HB�SB�dB�uB�|BʂBˇB̍B͒B͖BΛBΚBϠBѰB��B�B�(B�FB�nB�B��B��B��B��B��B��B	 �B	�B	�B	�B	�B	�B	�B	�B		�B	B	B	B	#B	-B	CB	SB	ZB	RB	[B	�B	%�B	(�B	*�B	,�B	.�B	0�B	2�B	6
B	>7B	@EB	AMB	CYB	EdB	FjB	I}B	L�B	O�B	R�B	S�B	U�B	X�B	[�B	]�B	`B	f'B	g.B	i;B	kHB	n[B	obB	o_B	qoB	y�B	{�B	|�B	}�B	~�B	~�B	�B	��B	��B	��B	�B	�B	�B	� B	�-B	�<B	�JB	�LB	�LB	�WB	�^B	�nB	�vB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�"B	�&B	�.B	�?B	�EB	�PB	�YB	�aB	�\B	�fB	�eB	�cB	�kB	�qB	�lB	�sB	�sB	�B	͖B	͕B	͖B	ΛB	͗B	ϢB	ЦB	ϢB	ЫB	ѭB	ӹB	ԾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�B	��B	�B	�B	�B	�B	�%B	�(B	�,B	�+B	�)B	�,B	�FB	�SB	�[B	�hB	�gB	�cB	�\B	�VB	�RB	�`B	�_B	�ZB	�hB	�\B	�SB	�[B	�_B	�gB	�hB	�mB	�xB	�~B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
B
B
OB
 �B
+�B
2�B
</B
?=B
CXB
FmB
M�B
S�B
Z�B
`B
dB
i;B
lOB
piB
tB
w�B
z�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =1(+/-0), vertically averaged dS =0.001(+/-0.001) in PSS-78.                                                                                                                                                                                                  Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040939592019060409395920190604093959  AO  ARCAADJP                                                                    20160124201653    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160124201653  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160124201653  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604093959  IP                  G�O�G�O�G�O�                