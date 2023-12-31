CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:08:55Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170855  20220204114412  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؇���1   @؇)��,@7�G�z��c�dZ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@�  A   A   A@  A`  A�  A�  A�33A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`��Bh  Bp  Bw��B�  B���B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D  Dy�D  D� D��Dy�D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D.��D/� D0  D0� D1  D1� D2  D2� D3fD3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D>��D?y�D@  D@y�D@��DAy�DA��DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt�fDt�fDy��D�ȤD�^�D�� D���D��D�X D��D��=D�D�V�D���D�ФD��D�W
Dڑ�D�޸D��D�MqD�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�\)@�@�A�HA>�HA^�HA~�HA�p�A���A���A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB`�Bg�RBo�RBwQ�B�RB���B���B��)B��)B��)B��)B��)B��)B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C<�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
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
C��=C��
C��
C��
C��
C��
C��
C��
C��
C��=C��
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
C��C��
C��
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D�D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D�DuD��D{�D��D{�D��D{�D��D{�D��DuD��D{�D�DuD��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.�D/{�D/��D0{�D0��D1{�D1��D2{�D3�D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>�D?uD?��D@uD@�DAuDA�DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt��Dt��Dy�\D��gD�\{D���D�ҐD��D�U�D���D�� D��D�T{D���D��gD��D�T�Dڏ]D��{D�]D�K4D�gD�ι111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A��
A��
A��#A��/A��/A��;A��HA��HA��TA��HA��;A��;A԰!A�5?AӉ7A�~�Aө�Aӣ�A�dZA���AҺ^AҍPAС�A�t�A��A���A��A���A���A�I�A�=qA��TA���A�|�A��A�~�A���A��A��A�"�A�E�A�G�A��hA�VA��A��!A�Q�A�7LA��/A�-A��RA�E�A�n�A���A���A��A�=qA�bNA��uA�7LA���A��A��A� �A�ffA��\A�C�A�%A��A��mA�;dA�Q�A�&�A��hA���A��A�ȴA�x�A�r�A�%A��`A���A��A�;dA�t�A�-A�oA~�9A{ƨAw��AshsAp=qAnAm?}AjȴAf^5Ac��Aa�PA_G�A\�!AY�TAV�!AU�AU�AU33AT��AU�AUS�AU�AU�TAVbAUdZAS�PAR(�AP�DAO��AN�AM��AL�jAL1AK"�AJ�AH��AG�^AFJAE��AE/AD�`AD��AD��ADz�AC��AAƨA?��A?%A=�mA<��A<�jA<��A<�DA<bNA;��A7�;A5l�A41A2�A2r�A2Q�A1��A1�-A01'A.v�A,�HA,VA,JA+��A++A*$�A)S�A)&�A(bNA'A%�^A%t�A%dZA%33A$r�A#��A#`BA"��A"��A"r�A"5?A!��A!�A!
=A �\Al�A�A��AVA�\AA`BAI�Ar�AoA��At�A1A��A|�A\)AĜA��AA��Az�A�;A��A�;A�A
�jA	�
A��A��A��AhsA��AĜA�+A=qA�wA �A\)A �R@��@�ȴ@��@�33@���@��D@��F@�p�@�+@��/@�!@�`B@���@�D@�A�@�&�@���@�h@�  @㝲@�K�@���@��@��@�7@�/@�9@�~�@�G�@��
@��y@�ff@ى7@�A�@�33@ա�@Լj@Ӯ@��@�9X@ϕ�@�n�@�7L@�r�@�S�@ʇ+@ə�@ț�@�+@��@���@�n�@�-@���@��/@�|�@��#@�1'@��F@�"�@��H@�{@���@���@���@�V@��-@�z�@��w@��y@�$�@���@���@�(�@���@��@�A�@���@�~�@�~�@�v�@�~�@�ff@�$�@�J@�&�@�bN@�1'@���@���@��+@�^5@�J@���@�V@�Ĝ@��@�A�@��w@��@��@�Ĝ@�ƨ@�|�@�"�@�$�@�O�@���@��@��@�I�@��;@�t�@�l�@�;d@��H@��@�@��+@�J@���@��@��@��m@��w@�\)@��@��H@���@�ȴ@��R@��!@��\@�^5@�V@�^5@�~�@�V@�E�@�E�@�=q@�{@���@��^@���@�x�@�X@�?}@�&�@�%@��@��@��`@���@��9@��u@� �@���@�o@��H@�~�@�5?@��^@�hs@��@���@���@���@���@���@�r�@�Z@�1'@�b@���@��F@���@��@�l�@�;d@��!@���@�^5@�{@��@���@��-@���@���@�hs@�O�@�X@�p�@�X@���@���@�A�@���@��m@��
@��
@��@��@�;d@��@�~�@�J@���@���@���@��-@��-@���@�p�@�7L@��9@�Q�@�9X@�b@���@��@�@��@���@�ȴ@�ȴ@���@��R@��!@���@���@��\@��+@��+@�M�@�$�@�J@��@�`B@�/@���@��@�Z@�1@��F@��@��@��@�t�@��@�
=@�"�@�+@���@��+@�E�@�-@�$�@��#@���@�X@���@���@�j@��@��m@���@z��@n�1@g��@_��@Y \@Q��@LXy@F҉@?��@;�{@4M@0V�@+�+@&��@!�D@]d@%�@��@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A���A���A���A���A���A���A��
A��
A��#A��/A��/A��;A��HA��HA��TA��HA��;A��;A԰!A�5?AӉ7A�~�Aө�Aӣ�A�dZA���AҺ^AҍPAС�A�t�A��A���A��A���A���A�I�A�=qA��TA���A�|�A��A�~�A���A��A��A�"�A�E�A�G�A��hA�VA��A��!A�Q�A�7LA��/A�-A��RA�E�A�n�A���A���A��A�=qA�bNA��uA�7LA���A��A��A� �A�ffA��\A�C�A�%A��A��mA�;dA�Q�A�&�A��hA���A��A�ȴA�x�A�r�A�%A��`A���A��A�;dA�t�A�-A�oA~�9A{ƨAw��AshsAp=qAnAm?}AjȴAf^5Ac��Aa�PA_G�A\�!AY�TAV�!AU�AU�AU33AT��AU�AUS�AU�AU�TAVbAUdZAS�PAR(�AP�DAO��AN�AM��AL�jAL1AK"�AJ�AH��AG�^AFJAE��AE/AD�`AD��AD��ADz�AC��AAƨA?��A?%A=�mA<��A<�jA<��A<�DA<bNA;��A7�;A5l�A41A2�A2r�A2Q�A1��A1�-A01'A.v�A,�HA,VA,JA+��A++A*$�A)S�A)&�A(bNA'A%�^A%t�A%dZA%33A$r�A#��A#`BA"��A"��A"r�A"5?A!��A!�A!
=A �\Al�A�A��AVA�\AA`BAI�Ar�AoA��At�A1A��A|�A\)AĜA��AA��Az�A�;A��A�;A�A
�jA	�
A��A��A��AhsA��AĜA�+A=qA�wA �A\)A �R@��@�ȴ@��@�33@���@��D@��F@�p�@�+@��/@�!@�`B@���@�D@�A�@�&�@���@�h@�  @㝲@�K�@���@��@��@�7@�/@�9@�~�@�G�@��
@��y@�ff@ى7@�A�@�33@ա�@Լj@Ӯ@��@�9X@ϕ�@�n�@�7L@�r�@�S�@ʇ+@ə�@ț�@�+@��@���@�n�@�-@���@��/@�|�@��#@�1'@��F@�"�@��H@�{@���@���@���@�V@��-@�z�@��w@��y@�$�@���@���@�(�@���@��@�A�@���@�~�@�~�@�v�@�~�@�ff@�$�@�J@�&�@�bN@�1'@���@���@��+@�^5@�J@���@�V@�Ĝ@��@�A�@��w@��@��@�Ĝ@�ƨ@�|�@�"�@�$�@�O�@���@��@��@�I�@��;@�t�@�l�@�;d@��H@��@�@��+@�J@���@��@��@��m@��w@�\)@��@��H@���@�ȴ@��R@��!@��\@�^5@�V@�^5@�~�@�V@�E�@�E�@�=q@�{@���@��^@���@�x�@�X@�?}@�&�@�%@��@��@��`@���@��9@��u@� �@���@�o@��H@�~�@�5?@��^@�hs@��@���@���@���@���@���@�r�@�Z@�1'@�b@���@��F@���@��@�l�@�;d@��!@���@�^5@�{@��@���@��-@���@���@�hs@�O�@�X@�p�@�X@���@���@�A�@���@��m@��
@��
@��@��@�;d@��@�~�@�J@���@���@���@��-@��-@���@�p�@�7L@��9@�Q�@�9X@�b@���@��@�@��@���@�ȴ@�ȴ@���@��R@��!@���@���@��\@��+@��+@�M�@�$�@�J@��@�`B@�/@���@��@�Z@�1@��F@��@��@��@�t�@��@�
=@�"�@�+@���@��+@�E�@�-@�$�@��#@���@�X@���@���@�j@��G�O�@���@z��@n�1@g��@_��@Y \@Q��@LXy@F҉@?��@;�{@4M@0V�@+�+@&��@!�D@]d@%�@��@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBv�Bv�Bv�Bv�Bv�Bw�Bw�Bw�Bv�Bx�Bx�Bx�By�Bx�Bx�Bw�By�Bz�B{�B��B�}B�)B�sB��BBB��BB"�B�B?}BK�BZBm�Bw�B}�B�%B�bB��B��B��B��B��B��B��B�oB�uB�bB�VB�DB�7B�B�B�B~�Bz�Bv�Br�Bp�BjBdZBbNB`BB^5BZBO�BI�BB�B7LB+B�BbBB�B�/B��B�XB��B�PBx�Bm�BbNBG�B/B�B	7B
��B
�/B
B
��B
�bB
�B
cTB
<jB
!�B
	7B	�B	ȴB	�-B	��B	�{B	� B	^5B	@�B	49B	!�B	�B	1B	  B��B	B	B	DB	hB	�B	�B	"�B	9XB	E�B	?}B	<jB	33B	.B	)�B	&�B	!�B	�B	�B	uB	PB	DB	PB	DB		7B	+B	%B	B	B��B�B�ZB�)B�B��B��B��B��B��B��B�}B��B��B��B��B��B��B��B�{B�hB�PB�DB�=B�1B�%B�B� B}�B� B� B� B|�B|�By�By�By�B� B~�B|�B|�B|�B{�Bz�Bz�B|�B}�Bw�Bu�Bt�Bo�Bl�BhsBdZB^5BVBR�BO�BN�BM�BN�BN�BO�BO�BL�BK�BL�BM�BK�BK�BI�BH�BF�BG�BD�BC�BB�BD�BA�B@�B?}B?}B@�B>wB>wB=qB<jB<jB;dB;dB:^B9XB;dB8RB7LB7LB5?B5?B49B2-B9XB;dB<jB=qB>wB>wB?}B?}B?}B?}B?}B>wB>wB>wBC�BC�BD�BE�BF�BE�BF�BH�BI�BL�BM�BM�BO�BP�BQ�BS�BS�BT�BT�BW
BW
BW
BW
BXBXB[#B\)B\)B[#B[#B\)B]/B^5BdZBjBjBk�Bl�Bn�Bp�Bq�Bs�Bt�Bt�Bu�Bv�Bw�B�B�B�B�B�B�B�B�B�B�1B�7B�7B�=B�VB�\B�\B�bB�oB��B��B��B��B��B��B��B��B�B�B�B�3B�LB�RB�^B�dB�qBÖBƨBȴBɺB��B��B�#B�TB�fB�sB�B��B��B��B��B��B��B��B��B��B	  B	B	B	B	%B	DB	DB	DB	PB	bB	{B	�B	 �B	"�B	$�B	&�B	'�B	(�B	+B	,B	-B	.B	/B	0!B	/B	0!B	33B	5?B	7LB	>wB	F�B	M�B	N�B	P�B	T�B	W
B	W
B	ZB	^5B	`BB	aHB	bNB	dZB	ffB	gmB	gmB	hsB	iyB	jB	m�B	m�B	n�B	q�B	r�B	s�B	t�B	w�B	x�B	{�B	{�B	|�B	� B	�B	�B	�%B	�%B	�+B	�7B	�=B	�=B	�DB	�VB	�VB	�VB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�3B	�9B	�?B	�?B	�FB	�FB	�RB	�XB	�^B	�^B	�jB	�jB	�wB	�}B	��B	��B	��B	ÖB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�#B	�#B	�/B	�5B	�5B	�5B	�;B	�HB	�NB	�TB	�B	��B
�B
�B
�B
$�B
-]B
3B
9�B
@�B
D�B
K�B
OBB
TFB
W�B
]�B
c�B
h�B
mwB
poB
rG111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Bm�Bm�Bm�Bm�Bm�Bn�Bn�Bn�Bm�Bo�Bo�Bo�Bp�Bo�Bo�Bn�Bp�Bq�Br�B��B��B�*B�sB��B�B�B��B�
B�B�B6xBB�BQBd�Bn�Bt�B}B�[B��B��B��B�zB��B��B�{B�iB�pB�]B�QB�@B�3B|B{BzBu�Bq�Bm�Bi�Bg�Ba~B[YBYNBWBBU5BQBF�B@�B9�B.OB"B�BhB�&B�B�9B��B�eB��B�`Bo�Bd�BYbB>�B&3B�B RB
��B
�MB
��B
��B
��B
x+B
Z|B
3�B
�B
 fB	�B	��B	�cB	��B	��B	w:B	UrB	7�B	+yB	B	�B�uB�EB�9B�QB�WB	�B	�B	�B	�B	B	0�B	<�B	6�B	3�B	*uB	%VB	!?B	,B	B	�B	�B	
�B	�B	�B	�B	�B	 }B�qB�kB�eB�RB�.B��BۣB�sB�NB�B�B�7B�BB�<B�$B��B�JB�+B��B��B��B��B��B��B��B��B��B��B�B}wBy^BwRBuFBwRBwRBwSBtABtABq.Bq.Bq.BwSBvMBtABtABtABs:Br4Br4BtABuGBo#BmBlBf�Bc�B_�B[�BU�BM[BJJBG7BF1BE+BF1BF1BG7BG7BD&BC BD&BE,BC BC BAB@B>B?B;�B:�B9�B;�B8�B7�B6�B6�B7�B5�B5�B4�B3�B3�B2�B2�B1�B0�B2�B/�B.�B.�B,�B,�B+�B)�B0�B2�B3�B4�B5�B5�B6�B6�B6�B6�B6�B5�B5�B5�B:�B:�B;�B= B>B= B>B@BABD+BE1BE1BG=BHCBIJBKVBKVBL\BL\BNhBNhBNhBNhBOnBOnBR�BS�BS�BR�BR�BS�BT�BU�B[�Ba�Ba�Bb�Bc�Be�BhBiBkBlBlBm Bn&Bo,BxbB{uB{uB{uB{uB{uB{uB|{B{uB�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�&B�KB�]B�cB�nB��B��B��B��B��B��B��B�B�B�B�B�JB�{BګBݽB��B��B�B�B�B�+B�7B�JB�JB�JB�PB�VB�aB�nB�tB�zB	�B	�B	�B	�B	�B	�B	 B	B	$B	0B	<B	CB	 IB	"UB	#[B	$aB	%gB	&nB	'tB	&nB	'tB	*�B	,�B	.�B	5�B	=�B	E$B	F*B	H6B	LNB	NZB	NZB	QmB	U�B	W�B	X�B	Y�B	[�B	]�B	^�B	^�B	_�B	`�B	a�B	d�B	d�B	e�B	h�B	i�B	kB	lB	oB	p#B	s5B	s5B	t<B	wNB	z`B	|lB	}rB	}rB	~xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�GB	�SB	�SB	�SB	�SB	�_B	�fB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�"B	�"B	�B	�"B	�.B	�5B	�;B	�MB	�SB	�fB	�lB	�lB	�lB	�wB	�}B	�}B	�}B	փB	ؐB	ٖG�O�B	�^B	�5B
B
0B
AB
B
$�B
*^B
1:B
7�B
;�B
B�B
F�B
K�B
O:B
U+B
[ B
_�B
d�B
g�B
i�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144122022020411441220220204114412  AO  ARCAADJP                                                                    20200619170855    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170855  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170855  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114412  IP                  G�O�G�O�G�O�                