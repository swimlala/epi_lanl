CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-07-26T09:15:52Z AOML 3.0 creation; 2016-08-07T21:36:37Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150726091552  20160807143637  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               >A   AO  5286_8897_062                   2C  D   APEX                            6531                            072314                          846 @�b��		1   @�bŎ8�@3�&�x���cF~��"�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    >A   B   B   @�33@�  A   A   A@  A`  A�  A���A���A�  A�33A�  A�  A�  B ffBffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dcy�Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy�3D�#3D�C3D���D�� D�3D�S3D�y�D���D�	�D�<�D�s3D�ٚD��D�<�DچfD�ɚD�3D�0 D�p D�6f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�A�HA"�HAB�HAb�HA�p�A�=qA�=qA�p�A���A�p�A�p�A�p�B�B	�B�RB�RB �RB(�RB0�RB8�RB@�RBH�RBP�RBX�RB`�RBh�RBp�RBx�RB�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)C .C.C.C.C.C
.C.C.C.C.C.C.C.C.C.C.C .C".C$.C&.C(.C*.C,.C..C0.C2.C4.C6.C8.C:.C<.C>.C@.CB.CD.CF.CH.CJ.CL.CN.CP.CR.CT.CV.CX.CZ.C\.C^.C`.Cb.Cd.Cf.Ch.Cj.Cl.Cn.Cp.Cr.Ct.Cv.Cx.Cz.C|.C~.C�
C�
C�
C�
C�
C�
C�
C�
C�#�C�#�C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�#�C�
C�
C�
C�
C�
C�
C�
C�
D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc�Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dtq�Dy��D�(�D�H�D���D���D��D�X�D�]D�ҐD�]D�B�D�x�D��]D��D�B�Dڌ)D��]D��D�5�D�u�D�<)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�
=A��/AۓuA�\)A�=qA�$�A�oA�%A�  A���A��A�~�Aأ�A�t�A�"�A��/Aԟ�A�1'A��;Aӏ\A���A�ĜA��A��AͲ-AˋDA�;dA�bNA�XA�O�Ać+A��A�E�A��wA�l�A���A�ƨA�A�x�A�hsA�|�A���A���A��yA��!A�1A�%A���A�K�A��
A�v�A�S�A��uA��#A��PA���A���A�l�A�Q�A���A�XA�A�`BA���A��+A��A��
A�E�A�{A��A�ĜA���A�v�A���A��PA�z�A�K�A�1A��A�33A��/A��A�/A��A��HA���A���A���A�1'A���A��A��A�33A��A�9XA��A�?}A��TA���A�"�A���A�7LA�/A��9A��A~��A~1A{Ay��AxbNAw�^Aul�Ar��Am��Ak`BAg�wAdĜAc��Ab��A]XA\5?AZ��AX�DAV{AT�AS�TASK�ARA�AN�AL=qAJQ�AIp�AG��AE�TACXA@��A=`BA<9XA;l�A;A:ȴA:�DA9��A6�9A4�HA3�A2n�A2$�A1ƨA1�A/S�A.1A+�A*bNA)��A)\)A(Q�A'�A&��A&�A$r�A#l�A" �A��A&�A��A��AVA-A�A�A�A�HAffA�wA��A9XA��A5?A��AQ�A^5A�wA�PA�hA��A�A
��A	ƨA	�FA	XA/A�7A&�A��A�wA��A�hA ��A ffA VA;dA�A?}A ��A v�@��@��H@���@���@�X@���@�V@�P@�F@��@�;d@��@�ƨ@�j@�z�@�@�"�@��@�(�@�7@�Ĝ@���@���@�7L@�\)@�M�@�$�@���@�?}@�I�@�t�@�S�@�p�@�u@�o@� �@��@�x�@�h@�j@�X@�A�@�33@���@ش9@�ƨ@ְ!@�`B@���@�z�@�j@ԃ@��H@�{@�hs@��y@�X@�@�;d@�
=@ް!@�/@�33@��@�v�@��@�Q�@�dZ@�7L@���@�K�@��@�~�@���@���@�t�@���@Ѳ-@��T@�Q�@̛�@��`@�r�@�j@˾w@�Z@Ͼw@�\)@ͩ�@���@��@�dZ@���@�Ĝ@̋D@̃@�j@�(�@���@ˍP@�
=@�E�@ɉ7@Ȭ@�Q�@�b@�r�@�Q�@� �@Ǖ�@��@���@�7L@��@���@Ĵ9@��m@Õ�@�dZ@�33@��@�n�@��@��u@�(�@�S�@��@�^5@�p�@�X@�G�@��@�r�@�Q�@��@��y@��-@��@��/@��/@��`@���@�7L@�G�@���@�M�@��@�hs@��^@�(�@��@��y@��@��@���@���@���@�n�@�v�@��!@���@�~�@�^5@���@�x�@��@�Z@�  @��
@��
@���@�l�@�33@���@�E�@�v�@�V@�=q@�{@��h@���@��@�@�@�l�@��@��@��
@�dZ@�+@���@�V@�p�@�O�@��@��9@�Ĝ@��@��F@���@�t�@���@�1@�j@�1'@�  @���@�@�J@���@��^@��h@�A�@��w@�33@��y@���@�~�@�^5@�M�@�-@��@�@���@�p�@��@��u@�(�@�b@��
@���@�S�@��H@���@�^5@��@��^@�`B@�O�@�G�@�/@���@���@�Ĝ@�Q�@��;@��F@���@�|�@�l�@�\)@�C�@��@���@��y@���@�ȴ@���@���@��\@�M�@�{@��T@���@��h@�X@�?}@�/@��@��@��@�9X@�  @���@��m@��w@�\)@�;d@��@�n�@�E�@�J@�ƨ@�~�@���@��@p��@i7L@`��@W��@Q�^@J�!@B�@:��@3S�@.@'��@#C�@�R@o@v�@J@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�
=A��/AۓuA�\)A�=qA�$�A�oA�%A�  A���A��A�~�Aأ�A�t�A�"�A��/Aԟ�A�1'A��;Aӏ\A���A�ĜA��A��AͲ-AˋDA�;dA�bNA�XA�O�Ać+A��A�E�A��wA�l�A���A�ƨA�A�x�A�hsA�|�A���A���A��yA��!A�1A�%A���A�K�A��
A�v�A�S�A��uA��#A��PA���A���A�l�A�Q�A���A�XA�A�`BA���A��+A��A��
A�E�A�{A��A�ĜA���A�v�A���A��PA�z�A�K�A�1A��A�33A��/A��A�/A��A��HA���A���A���A�1'A���A��A��A�33A��A�9XA��A�?}A��TA���A�"�A���A�7LA�/A��9A��A~��A~1A{Ay��AxbNAw�^Aul�Ar��Am��Ak`BAg�wAdĜAc��Ab��A]XA\5?AZ��AX�DAV{AT�AS�TASK�ARA�AN�AL=qAJQ�AIp�AG��AE�TACXA@��A=`BA<9XA;l�A;A:ȴA:�DA9��A6�9A4�HA3�A2n�A2$�A1ƨA1�A/S�A.1A+�A*bNA)��A)\)A(Q�A'�A&��A&�A$r�A#l�A" �A��A&�A��A��AVA-A�A�A�A�HAffA�wA��A9XA��A5?A��AQ�A^5A�wA�PA�hA��A�A
��A	ƨA	�FA	XA/A�7A&�A��A�wA��A�hA ��A ffA VA;dA�A?}A ��A v�@��@��H@���@���@�X@���@�V@�P@�F@��@�;d@��@�ƨ@�j@�z�@�@�"�@��@�(�@�7@�Ĝ@���@���@�7L@�\)@�M�@�$�@���@�?}@�I�@�t�@�S�@�p�@�u@�o@� �@��@�x�@�h@�j@�X@�A�@�33@���@ش9@�ƨ@ְ!@�`B@���@�z�@�j@ԃ@��H@�{@�hs@��y@�X@�@�;d@�
=@ް!@�/@�33@��@�v�@��@�Q�@�dZ@�7L@���@�K�@��@�~�@���@���@�t�@���@Ѳ-@��T@�Q�@̛�@��`@�r�@�j@˾w@�Z@Ͼw@�\)@ͩ�@���@��@�dZ@���@�Ĝ@̋D@̃@�j@�(�@���@ˍP@�
=@�E�@ɉ7@Ȭ@�Q�@�b@�r�@�Q�@� �@Ǖ�@��@���@�7L@��@���@Ĵ9@��m@Õ�@�dZ@�33@��@�n�@��@��u@�(�@�S�@��@�^5@�p�@�X@�G�@��@�r�@�Q�@��@��y@��-@��@��/@��/@��`@���@�7L@�G�@���@�M�@��@�hs@��^@�(�@��@��y@��@��@���@���@���@�n�@�v�@��!@���@�~�@�^5@���@�x�@��@�Z@�  @��
@��
@���@�l�@�33@���@�E�@�v�@�V@�=q@�{@��h@���@��@�@�@�l�@��@��@��
@�dZ@�+@���@�V@�p�@�O�@��@��9@�Ĝ@��@��F@���@�t�@���@�1@�j@�1'@�  @���@�@�J@���@��^@��h@�A�@��w@�33@��y@���@�~�@�^5@�M�@�-@��@�@���@�p�@��@��u@�(�@�b@��
@���@�S�@��H@���@�^5@��@��^@�`B@�O�@�G�@�/@���@���@�Ĝ@�Q�@��;@��F@���@�|�@�l�@�\)@�C�@��@���@��y@���@�ȴ@���@���@��\@�M�@�{@��T@���@��h@�X@�?}@�/@��@��@��@�9X@�  @���@��m@��w@�\)@�;d@��@�n�@�E�G�O�@�ƨ@�~�@���@��@p��@i7L@`��@W��@Q�^@J�!@B�@:��@3S�@.@'��@#C�@�R@o@v�@J@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
!�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
�VBBW
B{�B�VB�-BɺB�B�B8RBbB+B�B)�BZBbNBo�By�B� B��B�9B�RB��B�B��B��B��B��B�BÖB]/B  Br�B��BbB�B�B�B�B�B�B�B�B�B�B!�B#�B#�B!�B �B �B�B�B�B�B�B�B�BoBDB��B��B�B�sB�/B��B�'B�1Bu�Bo�Bo�BhsBZB=qB'�BhB��B�B�TB��B�{BiyBC�B1'B(�B�B%B
�B
�NB
�qB
��B
�JB
dZB
S�B
L�B
B�B
,B
{B
%B	��B	�sB	��B	��B	�=B	l�B	VB	K�B	@�B	�B	{B	DB��B��B�B�B�B�B�`B�/B�B��B��B��BȴBÖBɺBɺBɺBȴBȴBƨBBBBÖBȴB��B�/B�TB�yB�fB�B�B�B�B�B��B��B��B�B�B�B�BB��B��BɺBȴBǮBǮBǮBǮBȴBĜBB��B�wB�^B�?B�-B�B�B�B�!B�-B�9B�XBB�}B��B��B��B�jBȴB��BŢBĜB��B�wB��B�#B�B��B	PB	PB	
=B	1B	B��B��B�B�B�B	+B	�B	 �B	"�B	#�B	1'B	9XB	>wB	?}B	@�B	=qB	9XB	0!B	2-B	9XB	5?B	/B	+B	49B	9XB	9XB	8RB	8RB	7LB	9XB	E�B	E�B	A�B	K�B	E�B	F�B	H�B	D�B	;dB	7LB	6FB	33B	1'B	/B	/B	0!B	0!B	1'B	49B	:^B	M�B	e`B	e`B	r�B	�B	�uB	��B	��B	��B	�hB	�=B	�1B	�DB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�=B	�\B	�bB	�hB	�bB	��B	�9B	�3B	�B	��B	��B	��B	�'B	�dB	�dB	�dB	�qB	�wB	��B	��B	��B	ÖB	B	B	ÖB	ĜB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	ȴB	ǮB	ÖB	�RB	�'B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�?B	�FB	�FB	�FB	�FB	�LB	�XB	�^B	�^B	�dB	�qB	�qB	�jB	�wB	��B	ĜB	ĜB	ĜB	ŢB	ĜB	ŢB	��B	��B	�B	�)B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�/B	�TB	�`B	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	�fB	�`B	�fB	�fB	�fB	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
bB
�B
�B
�B
$�B
'�B
)�B
33B
<jB
B�B
H�B
L�B
Q�B
VB
[#B
_;B
cTB
hsB
l�B
p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
�MB �BV�B{�B�GB�BɪB�|B�B8EBTBBsB)�BZBbCBo�By�B�B��B�.B�FB˹B��B��B˺BʴB��B��BÅB] B��Br�B��BTB�B�B�BsB�B�B�B�B�B�B!�B#�B#�B!�B �B �B�B�B�B�B�B�B{B`B3B��B��B�B�fB�#BʳB�B�Bu�Bo�Bo�BhbBZB=cB'�BZB��B�xB�BB�rB�hBihBC�B1B(�B�BB
�B
�>B
�cB
��B
�=B
dNB
S�B
L�B
B�B
+�B
nB
B	��B	�iB	ʸB	��B	�8B	l�B	U�B	K�B	@�B	�B	xB	@B��B��B�B�B�B�B�]B�.B�B��B��B��BȴBÖBɹBɸBɸBȴBȴBƧBBBBÕBȲB��B�,B�QB�vB�cB�B�B�B�B�B��B��B��B�B�B�B�@B��B��BɹBȱBǩBǫBǫBǩBȲBĘBB�B�uB�\B�=B�,B�B�B�B�B�.B�6B�TBB�zB��B��B��B�eBȱB��BŠBęB�~B�tB��B�B�B��B	JB	IB	
4B	*B	B��B��B�B�B�B	$B	~B	 �B	"�B	#�B	1B	9NB	>mB	?tB	@yB	=gB	9OB	0B	2#B	9MB	55B	/B	*�B	4/B	9OB	9OB	8HB	8KB	7BB	9OB	E�B	E�B	A�B	K�B	E�B	F�B	H�B	D�B	;[B	7DB	6;B	3*B	1B	/B	/B	0B	0B	1B	40B	:WB	M�B	eVB	eUB	r�B	�B	�hB	��B	��B	�zB	�[B	�0B	�&B	�8B	��B	��B	��B	�B	�uB	�{B	�B	��B	��B	�vB	�|B	�sB	��B	��B	�hB	�0B	�PB	�WB	�[B	�VB	��B	�)B	�$B	��B	��B	��B	��B	�B	�XB	�XB	�WB	�eB	�iB	�xB	�tB	�uB	ÈB	B	B	ÆB	ČB	ȤB	ȤB	ɫB	ʲB	˸B	̼B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	̿B	ȦB	ȤB	̾B	��B	��B	��B	��B	��B	̾B	ȥB	ǟB	ÈB	�EB	�B	�B	�B	� B	�B	�B	�B	�B	�B	�B	�B	�1B	�8B	�7B	�7B	�6B	�>B	�KB	�PB	�OB	�TB	�bB	�aB	�\B	�jB	�vB	čB	ĎB	ĎB	ŕB	čB	ŔB	ʱB	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	� B	�B	�!B	�DB	�NB	�vB	�{B	�|B	�{B	�|B	�pB	�oB	�lB	�cB	�XB	�PB	�WB	�UB	�UB	�^B	�`B	�]B	�]B	�eB	�dB	�bB	�gB	�hB	�jB	�lB	�lB	�mB	�nB	�uB	�zB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
 �B
�G�O�B
B
PB
�B
�B
{B
$�B
'�B
)�B
3$B
<TB
B~B
H�B
L�B
Q�B
U�B
[B
_)B
c@B
h_B
lxB
p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.18 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436372016080714363720160807143637  AO  ARCAADJP                                                                    20150726091552    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150726091552  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150726091552  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143637  IP                  G�O�G�O�G�O�                