CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:17:35Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190219181735  20200831164653  5903273 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  3334                            2C  D   APEX                            4917                            041310                          846 @֩f��H�1   @֩g5�L@6YXbM��b��/��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX  B`  Bh  Bp  Bx  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Do��Dpy�Dp��Dqy�Dr  Dr� Ds  Ds� Ds��Dty�Dy{�D�fD�MD��RD���D��D�7\D�x�D��{D���D�C�D��
D��3D�fD�A�Dڃ3D���D���D�6�D�p�D�b�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @E@�G�@�G�A��A$��AD��Ad��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A��A�Q�B(�B	(�B(�B(�B!(�B)(�B1(�B9(�BA(�BI(�BQ�\BY(�Ba(�Bi(�Bq(�By(�B��{B�ǮB��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BĔ{BȔ{B̔{BД{BԔ{Bؔ{Bܔ{B��{B�{B�{B�{B�{B��{B��{B��{C J=CJ=CJ=CJ=CJ=C
c�CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=C J=C"J=C$J=C&J=C(J=C*J=C,J=C.J=C0J=C2J=C4J=C6J=C8J=C:J=C<J=C>J=C@J=CBJ=CDJ=CFJ=CHJ=CJJ=CLJ=CNJ=CPJ=CRJ=CTJ=CVJ=CXJ=CZJ=C\J=C^J=C`J=CbJ=CdJ=CfJ=ChJ=CjJ=ClJ=CnJ=Cpc�CrJ=CtJ=CvJ=CxJ=CzJ=C|J=C~J=C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp)Dp�)Dq)Dq�)Dr�Dr��Ds�Ds��Dt)Dt�)Dy�D��D�VgD���D��4D���D�@�D��>D���D��D�L�D��RD��{D��D�J�Dڌ{D��>D��4D�@ D�z>D�k�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aʗ�Aʗ�AʍPAʅAʗ�Aʙ�Aʛ�Aʕ�Aʕ�Aʗ�Aʕ�Aʗ�Aʗ�AʓuAʑhAʓuAʑhAʑhAʑhAʑhAʏ\AʋDA�~�A�r�A�hsA�A�A�A��;A�x�A���A�VA��A��A��/A��#A�ffA���A�M�A�A��^A��^A�bA�ƨA���A�%A��A��;A�;dA�jA���A��A���A��yA���A��wA�v�A��A��
A���A�`BA�E�A��A���A���A�\)A�K�A��RA�r�A�A��yA�/A��A��mA�v�A���A�"�A��
A�S�A��TA�hsA��HA��^A�dZA�-A�(�A�l�A��DA��FA��yA�"�A�
=A�XA�oA��A���A���A��!A���A��A���A�"�A��!A�^5A��DA�I�A�O�A�;dA��A��A���A��A�1'A}dZAzr�Ax��AvbAsl�Ao��Aln�Aj~�AhM�Af��Ae�Ad�!Ac�A`�A_A]"�AY��AWp�AV �AS�;AQl�AN��AN�AM�ALVAG%AE�FAE�ADVAA�#AA?}A?|�A>�jA<�!A:�DA9dZA8ȴA7��A6ȴA6VA5��A4�DA2ȴA0z�A/��A-�A+dZA*�9A)x�A'��A&z�A%�wA%C�A$�`A$�DA$1'A#�A#�^A"��A �A��AVAM�A-A�Ax�AM�A�;AVA�A��AA�A�FA�A~�AffA
=A��A��AoA^5A��AC�A
=A�#A��Ap�AA
1A	33A��AjAVA  A|�A
=A�\At�A7LA�+A�
A;dAr�A`B@��
@�5?@��@�C�@�=q@��u@�$�@��@�l�@�x�@��@�5?@�j@�o@�@�X@�A�@�5?@��@�  @�R@���@��@�C�@���@�M�@܃@�1'@��@��
@ە�@�n�@ّh@�/@���@؃@�K�@�?}@���@�bN@�ƨ@�l�@�ȴ@���@��@�33@�=q@ͩ�@�ƨ@�33@�"�@�$�@ț�@���@�t�@��@�z�@���@Å@�C�@�o@�o@�^5@�1'@���@�+@�@�p�@���@���@�33@��@�&�@���@��@��D@�bN@�9X@��@�  @�|�@���@���@���@��D@�(�@���@��7@�?}@�/@���@��@���@�33@���@���@�M�@��@�`B@�%@��D@�1'@�1@���@�|�@��y@�~�@�@���@���@��@��@�Ĝ@���@��w@�ff@��-@�x�@�hs@�G�@��@��@���@�I�@�9X@�9X@��@��m@��;@��P@���@�
=@�l�@�33@�@��!@��\@�v�@�=q@�@��h@��@�Ĝ@��@�z�@�Q�@��@��P@�33@��@��!@�$�@�@��7@�x�@�hs@��@�p�@�hs@�X@�?}@���@�j@�A�@��@�C�@�
=@��R@��@�x�@���@�j@�+@�-@��@�A�@�z�@���@��D@�9X@�ƨ@��@�ȴ@�@���@��@���@��\@��H@���@��@�ȴ@���@��R@���@���@�ȴ@��H@�
=@�"�@�+@�+@��y@�-@�@�J@���@�J@�J@���@�%@�z�@�b@��@��@�(�@�(�@�b@���@���@��w@��@��P@��P@��F@���@�
=@���@��\@�ȴ@��+@�{@��+@���@��@��@�x�@��@�O�@�&�@��@��@�Z@� �@�o@���@���@��@���@�n�@�5?@��^@�O�@�X@�X@�X@��@���@���@��/@��9@�Q�@��
@�|�@�K�@�C�@�+@�o@�ȴ@���@�n�@�E�@�=q@���@|��@r�6@h�9@_��@X�O@R��@M��@C�	@>�X@9@3o�@/RT@)rG@$oi@�@-@��@{�@C�@M111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Aʗ�Aʗ�AʍPAʅAʗ�Aʙ�Aʛ�Aʕ�Aʕ�Aʗ�Aʕ�Aʗ�Aʗ�AʓuAʑhAʓuAʑhAʑhAʑhAʑhAʏ\AʋDA�~�A�r�A�hsA�A�A�A��;A�x�A���A�VA��A��A��/A��#A�ffA���A�M�A�A��^A��^A�bA�ƨA���A�%A��A��;A�;dA�jA���A��A���A��yA���A��wA�v�A��A��
A���A�`BA�E�A��A���A���A�\)A�K�A��RA�r�A�A��yA�/A��A��mA�v�A���A�"�A��
A�S�A��TA�hsA��HA��^A�dZA�-A�(�A�l�A��DA��FA��yA�"�A�
=A�XA�oA��A���A���A��!A���A��A���A�"�A��!A�^5A��DA�I�A�O�A�;dA��A��A���A��A�1'A}dZAzr�Ax��AvbAsl�Ao��Aln�Aj~�AhM�Af��Ae�Ad�!Ac�A`�A_A]"�AY��AWp�AV �AS�;AQl�AN��AN�AM�ALVAG%AE�FAE�ADVAA�#AA?}A?|�A>�jA<�!A:�DA9dZA8ȴA7��A6ȴA6VA5��A4�DA2ȴA0z�A/��A-�A+dZA*�9A)x�A'��A&z�A%�wA%C�A$�`A$�DA$1'A#�A#�^A"��A �A��AVAM�A-A�Ax�AM�A�;AVA�A��AA�A�FA�A~�AffA
=A��A��AoA^5A��AC�A
=A�#A��Ap�AA
1A	33A��AjAVA  A|�A
=A�\At�A7LA�+A�
A;dAr�A`B@��
@�5?@��@�C�@�=q@��u@�$�@��@�l�@�x�@��@�5?@�j@�o@�@�X@�A�@�5?@��@�  @�R@���@��@�C�@���@�M�@܃@�1'@��@��
@ە�@�n�@ّh@�/@���@؃@�K�@�?}@���@�bN@�ƨ@�l�@�ȴ@���@��@�33@�=q@ͩ�@�ƨ@�33@�"�@�$�@ț�@���@�t�@��@�z�@���@Å@�C�@�o@�o@�^5@�1'@���@�+@�@�p�@���@���@�33@��@�&�@���@��@��D@�bN@�9X@��@�  @�|�@���@���@���@��D@�(�@���@��7@�?}@�/@���@��@���@�33@���@���@�M�@��@�`B@�%@��D@�1'@�1@���@�|�@��y@�~�@�@���@���@��@��@�Ĝ@���@��w@�ff@��-@�x�@�hs@�G�@��@��@���@�I�@�9X@�9X@��@��m@��;@��P@���@�
=@�l�@�33@�@��!@��\@�v�@�=q@�@��h@��@�Ĝ@��@�z�@�Q�@��@��P@�33@��@��!@�$�@�@��7@�x�@�hs@��@�p�@�hs@�X@�?}@���@�j@�A�@��@�C�@�
=@��R@��@�x�@���@�j@�+@�-@��@�A�@�z�@���@��D@�9X@�ƨ@��@�ȴ@�@���@��@���@��\@��H@���@��@�ȴ@���@��R@���@���@�ȴ@��H@�
=@�"�@�+@�+@��y@�-@�@�J@���@�J@�J@���@�%@�z�@�b@��@��@�(�@�(�@�b@���@���@��w@��@��P@��P@��F@���@�
=@���@��\@�ȴ@��+@�{@��+@���@��@��@�x�@��@�O�@�&�@��@��@�Z@� �@�o@���@���@��@���@�n�@�5?@��^@�O�@�X@�X@�X@��@���@���@��/@��9@�Q�@��
@�|�@�K�@�C�@�+@�o@�ȴ@���@�n�@�E�G�O�@���@|��@r�6@h�9@_��@X�O@R��@M��@C�	@>�X@9@3o�@/RT@)rG@$oi@�@-@��@{�@C�@M111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBjBiyBiyBiyBjBjBjBjBjBjBjBjBk�Bl�Bl�Bl�Bm�Bm�Bm�Bn�Bo�Bo�Bp�Bq�Bp�Bw�B�VB��B�!B�XB�?B��B�BŢB��BɺBB6FB
=BB\B/BN�B|�B��B�}BVB>wBF�B:^B,B-B49B5?B\)Be`BdZBs�B|�B{�Br�Bq�Bp�BcTB7LBN�BQ�B]/Be`Bk�BiyBD�B9XB �BB%�B�BoB	7B�sB�B��B�B[#B=qB+B�B\BB�B�;B��B�qB�B��B��B��Bn�B9XB �BoB
�B
�dB
��B
�\B
bNB
O�B
9XB
$�B
�B
JB	�B	�B	��B	�^B	��B	�bB	v�B	^5B	VB	N�B	F�B	A�B	9XB	1'B	 �B	�B	B�B�B�fB�)B�B��B��B��BB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�{B�hB�DB�B�+B�+B�B�B�B�B�B�B~�Bu�Br�Bq�Bp�Bo�Bn�Bm�Bk�BiyBffBdZBcTBaHB_;B^5B]/B[#BYBXBW
BT�BR�BQ�BP�BN�BJ�BH�BE�BC�BC�BC�BD�BD�BC�BC�BC�BC�BB�BA�B?}B=qB<jB:^B8RB9XB9XB9XB9XB:^B9XB7LB8RB8RB7LB5?B33B1'B/B/B0!B0!B/B-B-B,B-B.B-B2-B5?B7LB=qB=qB=qB=qB<jB=qB>wB?}B>wB>wB@�BD�BD�BD�BE�BE�BE�BF�BF�BH�BI�BI�BK�BL�BK�BL�BO�BP�BP�BVB`BBcTBffBhsBjBiyBl�Bx�B{�B~�B�B�%B�+B�1B�DB�\B�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B�B�'B�3B�9B�3B�FB�XB�jB�jB�wB��B��BĜBƨBƨBǮBǮBǮBǮB��B��B��B��B��B��B�B�5B�HB�ZB�sB�B�B�B�B�B�B��B��B	  B	B	%B	+B	\B	hB	{B	�B	�B	"�B	!�B	"�B	"�B	#�B	$�B	&�B	-B	49B	7LB	7LB	8RB	:^B	<jB	>wB	@�B	D�B	H�B	I�B	M�B	M�B	N�B	O�B	S�B	VB	[#B	\)B	_;B	`BB	`BB	aHB	ffB	iyB	iyB	jB	l�B	m�B	q�B	t�B	r�B	p�B	q�B	u�B	� B	�B	�B	�B	�B	�B	�+B	�=B	�PB	��B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�!B	�'B	�-B	�?B	�LB	�dB	�dB	�dB	�^B	�XB	�XB	�dB	��B	ÖB	ĜB	ĜB	ĜB	ƨB	ƨB	ƨB	ƨB	ǮB	ɺB	ȴB	ȴB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�/B	�/B	�5B	�5B	�;B	�BB	�BB	�BB	�HB	�NB	�ZB	�ZB	�fB	�fB	�`B	�fB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	��B	��B
�B
�B
"�B
*KB
2�B
72B
AUB
E�B
LJB
Q4B
TFB
ZkB
^�B
dZB
i_B
mwB
q�B
u?B
x111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BjdBi`BibBicBjeBjfBjfBjfBjeBjeBjfBjfBknBlrBltBltBmyBmwBmuBnBo�Bo�Bp�Bq�Bp�Bw�B�AB��B�B�?B�'B��B��BŊB�qBɤBB6-B
'BB?B/BN�B|�B��B�eB>B>]BF�B:GB+�B,�B4B5)B\BeDBd@Bs�B|�B{�Br�Bq�Bp�Bc:B73BN�BQ�B]BeFBkkBi^BD�B9:B �B�B%�B�BUB	B�ZB��B��B��B[	B=ZB*�B�B>B �B�B�BνB�UB��B��B��B�}Bn{B9:B �BRB
�B
�EB
��B
�BB
b1B
O�B
9<B
$�B
�B
+B	�B	��B	�hB	�?B	��B	�FB	v�B	^B	U�B	N�B	F�B	AjB	9:B	1B	 �B	hB	�B�B�qB�GB�	B��B��BλB˨B�qB��B��B��B��B��B��B��B��B�lB��B��B�qB�rB��B��B�zB�mB�mB�rB�fB�OB�[B�HB�$B��B�	B�
B��B��B��B��B��B��B~�Bu�Br�Bq�Bp�Bo|BnwBmoBkbBiVBfEBd8Bc0Ba%B_B^B]B[BX�BW�BV�BT�BR�BQ�BP�BN�BJ�BH�BE�BCtBCsBCrBDxBDyBCsBCuBCsBCuBBmBAfB?XB=LB<GB:8B8/B95B94B97B95B:;B95B7(B8.B8.B7(B5B3B1B.�B.�B/�B/�B.�B,�B,�B+�B,�B-�B,�B2
B5B7(B=MB=KB=LB=KB<EB=LB>RB?YB>QB>QB@^BDxBDwBDxBE}BE~BE�BF�BF�BH�BI�BI�BK�BL�BK�BL�BO�BP�BP�BU�B`Bc0BfBBhPBj[BiSBlfBx�B{�B~�B��B��B�B�B� B�9B�JB�QB�VB�ZB�eB�mB�mB�uB�zB��B��B��B��B��B��B�B�B�B�B�!B�4B�EB�FB�QB�^B�dB�tBƂBƁBǊBǉBǈBǉBʛBˣBδBαB��B��B��B�B�#B�4B�OB�_B�lB�lB�qB�vB�B��B��B��B	 �B	�B	B	6B	AB	VB	yB	�B	"�B	!�B	"�B	"�B	#�B	$�B	&�B	,�B	4B	7'B	7%B	8+B	:8B	<FB	>RB	@\B	DvB	H�B	I�B	M�B	M�B	N�B	O�B	S�B	U�B	Z�B	\B	_B	`B	`B	a%B	fAB	iSB	iSB	j\B	leB	mlB	q�B	t�B	r�B	p}B	q�B	u�B	�B	��B	��B	��B	��B	��B	�B	�B	�*B	�hB	�PB	�TB	�nB	�tB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�&B	�=B	�<B	�>B	�:B	�0B	�1B	�>B	�cB	�pB	�yB	�uB	�wB	ƃB	ƃB	ƃB	ƄB	ǇB	ɖB	ȍB	ȏB	ǇB	ˠB	ͫB	ͮB	ͫB	оB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�B	�B	�B	�#B	�)B	�4B	�4B	�>B	�=B	�:B	�?B	�GB	�HB	�MB	�LB	�RB	�WB	�_B	�`G�O�B	�|B	��B
�B
sB
"[B
*$B
2�B
7
B
A/B
E}B
L#B
QB
T B
ZEB
^xB
d4B
i9B
mNB
q�B
uB
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.29 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             202008311646532020083116465320200831164653  AO  ARCAADJP                                                                    20190219181735    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20190219181735  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20190219181735  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20200831164653  IP                  G�O�G�O�G�O�                