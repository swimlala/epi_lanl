CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:19Z creation      
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
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20200619170919  20220204114422  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               pA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @����.1   @��}'�@6�Q��c"n��O�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    pA   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B���B�  B�33B�ffB���B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � DfD� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,�fD-  D-� D.  D.y�D/  D/� D0fD0� D1  D1� D2  D2� D2��D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DLy�DM  DM� DN  DN� DO  DO� DP  DP� DQfDQ�fDR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� DlfDl� Dm  Dm� Dn  Dn�fDo  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy��D�)D�`RD��RD��D�fD�VfD��HD���D�
D�S�D��\D�ФD�'\D�S3DڕqD��RD�RD�\{D�\D�=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��\@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B�\B��)B��)B��)B���B��)B�\B�B�B���B��)B��)B��)B��)B��)B��)B�\B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
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
D {�D�D{�D�D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��DuD��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D�D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,��D,��D-{�D-��D.uD.��D/{�D0�D0{�D0��D1{�D1��D2{�D2�D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DLuDL��DM{�DM��DN{�DN��DO{�DO��DP{�DQ�DQ��DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dl�Dl{�Dl��Dm{�Dm��Dn��Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dy�)D��D�^D��D���D�)D�T)D��D�ۆD��D�Q�D��D��gD�%D�P�Dړ4D��D�D�Z>D�D� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ƨA�ƨA�ȴA�ȴA�ƨA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A�ĜA׾wA׺^Aװ!Aװ!Aװ!Aװ!Aװ!A׮A׮A׬A׬A׬A׬Aש�Aק�Aן�Aי�A׃A�A�A��A��HAд9A�=qA�n�A���Aʟ�A�(�A� �AĮA�JA�\)A� �A��mA��\A�x�A���A�VA���A��A�\)A�jA���A�K�A�ĜA��/A�-A�A���A��yA�1A�;dA���A�G�A���A���A���A���A��DA���A���A�dZA� �A�p�A��A���A�9XA���A�A�oA�G�A��`A�n�A��TA��TA�n�A�|�A���A��!A�x�A�ƨA�ffA��A��uA���A��A�?}A��A���A�%A�"�A��A�&�A��A�/A���A���A��yA�n�A}Ax��Av�At�uAr��Ap��AmC�Ai�AgAeG�Ac�A`�yA_p�A^��A\�HA[oAW��AU7LAS�;AR �APA�AN�HAMoAKO�AIhsAGl�AEp�AB�A@-A=�PA<ffA;��A;C�A:Q�A9�A933A7��A6�A57LA4ffA3/A1K�A0M�A/"�A.{A,�yA+
=A)�#A(v�A'O�A&�\A&=qA%ƨA$��A#��A"�A ��A��A9XA�A��A�+AE�A�PA��A�A�AE�AE�AbA�#A��A�A�-A�!A�7A�FA\)AdZA?}A�jAhsAĜA�AA
ĜA
I�A�A�yAv�AI�A�#A��A��A�
A;dA�A�@�v�@��/@�1@���@�v�@���@���@�?}@�K�@�M�@��/@���@�X@��@��@��H@�=q@�x�@��@�
=@�R@���@��/@㕁@��@�9X@��
@ߍP@޸R@��
@ٙ�@ؼj@��@�o@���@�@�7L@��@�1'@�^5@�Ĝ@��;@�Z@��#@�&�@��@�/@��@˾w@�J@ț�@ǥ�@��`@Ĵ9@�1@Å@�{@���@���@�  @��u@��^@�5?@���@�p�@�=q@���@�  @��#@�I�@�t�@���@��@��D@�+@��\@�A�@�I�@�t�@���@�(�@�j@���@��P@���@�"�@�`B@�Ĝ@�A�@���@�v�@�%@�bN@��
@��@�|�@��@���@���@��@��P@���@���@�G�@��@��`@�z�@�Z@�A�@�I�@�Q�@��u@��@��`@�Q�@�9X@�1@�"�@��+@�@��T@�$�@�C�@��y@�5?@���@�X@��@�z�@�Z@�t�@���@�J@���@�7L@���@��j@�Ĝ@��@�V@�M�@��-@��w@���@���@��F@�ƨ@�"�@��@��#@�@�n�@���@��/@���@��`@��@���@�|�@��@�o@�@��@�hs@�X@�%@���@��@�p�@�5?@�x�@��`@���@��@���@���@��w@�A�@�1@��F@��F@�ƨ@�K�@���@��+@�v�@�v�@�M�@�E�@��@���@��^@���@��-@���@���@���@��7@�O�@�/@�%@���@��`@�1@���@���@�\)@�C�@�33@�o@��y@��R@��\@�E�@�{@��@���@��-@��h@�X@�7L@�&�@��@���@��`@���@��@�z�@�Q�@�I�@� �@��@���@��@�l�@�dZ@�S�@�
=@��@�ȴ@��R@���@���@���@���@�~�@�E�@�-@�{@�@���@��#@���@��h@�G�@��@�%@��@���@���@��D@�r�@�I�@��m@���@���@�+@���@���@���@��+@�~�@�n�@���@x6@o�P@g~�@b6�@Z($@S��@N��@H��@CJ#@;�&@50�@/��@(�@#��@��@-w@C-@-�@�@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�ƨA�ƨA�ȴA�ȴA�ƨA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A�ĜA׾wA׺^Aװ!Aװ!Aװ!Aװ!Aװ!A׮A׮A׬A׬A׬A׬Aש�Aק�Aן�Aי�A׃A�A�A��A��HAд9A�=qA�n�A���Aʟ�A�(�A� �AĮA�JA�\)A� �A��mA��\A�x�A���A�VA���A��A�\)A�jA���A�K�A�ĜA��/A�-A�A���A��yA�1A�;dA���A�G�A���A���A���A���A��DA���A���A�dZA� �A�p�A��A���A�9XA���A�A�oA�G�A��`A�n�A��TA��TA�n�A�|�A���A��!A�x�A�ƨA�ffA��A��uA���A��A�?}A��A���A�%A�"�A��A�&�A��A�/A���A���A��yA�n�A}Ax��Av�At�uAr��Ap��AmC�Ai�AgAeG�Ac�A`�yA_p�A^��A\�HA[oAW��AU7LAS�;AR �APA�AN�HAMoAKO�AIhsAGl�AEp�AB�A@-A=�PA<ffA;��A;C�A:Q�A9�A933A7��A6�A57LA4ffA3/A1K�A0M�A/"�A.{A,�yA+
=A)�#A(v�A'O�A&�\A&=qA%ƨA$��A#��A"�A ��A��A9XA�A��A�+AE�A�PA��A�A�AE�AE�AbA�#A��A�A�-A�!A�7A�FA\)AdZA?}A�jAhsAĜA�AA
ĜA
I�A�A�yAv�AI�A�#A��A��A�
A;dA�A�@�v�@��/@�1@���@�v�@���@���@�?}@�K�@�M�@��/@���@�X@��@��@��H@�=q@�x�@��@�
=@�R@���@��/@㕁@��@�9X@��
@ߍP@޸R@��
@ٙ�@ؼj@��@�o@���@�@�7L@��@�1'@�^5@�Ĝ@��;@�Z@��#@�&�@��@�/@��@˾w@�J@ț�@ǥ�@��`@Ĵ9@�1@Å@�{@���@���@�  @��u@��^@�5?@���@�p�@�=q@���@�  @��#@�I�@�t�@���@��@��D@�+@��\@�A�@�I�@�t�@���@�(�@�j@���@��P@���@�"�@�`B@�Ĝ@�A�@���@�v�@�%@�bN@��
@��@�|�@��@���@���@��@��P@���@���@�G�@��@��`@�z�@�Z@�A�@�I�@�Q�@��u@��@��`@�Q�@�9X@�1@�"�@��+@�@��T@�$�@�C�@��y@�5?@���@�X@��@�z�@�Z@�t�@���@�J@���@�7L@���@��j@�Ĝ@��@�V@�M�@��-@��w@���@���@��F@�ƨ@�"�@��@��#@�@�n�@���@��/@���@��`@��@���@�|�@��@�o@�@��@�hs@�X@�%@���@��@�p�@�5?@�x�@��`@���@��@���@���@��w@�A�@�1@��F@��F@�ƨ@�K�@���@��+@�v�@�v�@�M�@�E�@��@���@��^@���@��-@���@���@���@��7@�O�@�/@�%@���@��`@�1@���@���@�\)@�C�@�33@�o@��y@��R@��\@�E�@�{@��@���@��-@��h@�X@�7L@�&�@��@���@��`@���@��@�z�@�Q�@�I�@� �@��@���@��@�l�@�dZ@�S�@�
=@��@�ȴ@��R@���@���@���@���@�~�@�E�@�-@�{@�@���@��#@���@��h@�G�@��@�%@��@���@���@��D@�r�@�I�@��m@���@���@�+@���@���@���@��+@�~�G�O�@���@x6@o�P@g~�@b6�@Z($@S��@N��@H��@CJ#@;�&@50�@/��@(�@#��@��@-w@C-@-�@�@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
`BB
aHB
aHB
_;B
^5B
^5B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
`BB
`BB
_;B
o�B
��B
t�B
r�B
��BH�BB�BF�Bt�B�7B��B�B��B�uB��B��BVB��B�HB�B�B�
B��B��B��B��B��B�NB�NB��B��B��B��B�BK�BQ�B\)BZBYBXBW
BVBS�BN�BL�BI�BJ�BB�B6FB$�B�B+B�yB�
BB�?B�9B�wB�B�{B|�B|�Bp�Be`BT�BH�B;dB/B�BVBB
�B
��B
�-B
��B
�%B
ffB
XB
G�B
 �B
%B	��B	�yB	�#B	B	�B	��B	�=B	|�B	p�B	iyB	bNB	`BB	XB	C�B	1'B	'�B	�B	{B	\B	B��B�B�NB��B��B��B��B�dB�LB�?B�-B�!B�B�B��B��B��B��B��B�bB�\B�DB�1B�B~�B� B}�Bz�By�By�Bw�Bu�Br�Bk�Be`BaHBVBW
BXB[#B[#B]/B\)B[#B[#BXBW
BW
BW
BYBZBW
BiyBy�Bz�B�B�B�%B�B|�B�B�B}�Bx�Br�B]/B[#B_;B^5BdZB\)B^5B_;BZB`BB]/BZB\)B`BB_;B[#BVBP�BM�BO�BS�BS�BP�BP�BP�BN�BK�BK�BJ�BI�BP�BS�BS�BZB\)B[#B_;B_;B]/B[#BW
BS�BO�BW
BVBW
BVB\)B\)B[#BW
BXB_;B]/BZBZBYB[#BZB\)B[#BZBVBW
BXBYBXB\)B_;B^5BgmBs�Bw�B|�B�+B�B�B�B�B�%B�DB�PB�oB�hB�bB�bB�VB��B��B��B��B��B��B��B�^B�jB�FB�FB�LB�FB�RB�?B�RB�^B�qB�}BÖB��B�B�
B��B��B��B��B��B��B�B�
B�B�)B�/B�BB�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B	  B	B	+B	DB	{B	�B	 �B	!�B	 �B	!�B	#�B	%�B	'�B	'�B	&�B	+B	+B	/B	49B	1'B	49B	8RB	9XB	9XB	:^B	>wB	A�B	D�B	F�B	H�B	J�B	M�B	P�B	T�B	YB	bNB	bNB	aHB	`BB	cTB	ffB	iyB	jB	o�B	s�B	u�B	v�B	z�B	{�B	{�B	z�B	{�B	|�B	}�B	~�B	�B	�B	�B	�%B	�+B	�7B	�JB	�JB	�\B	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�9B	�?B	�LB	�RB	�XB	�^B	�dB	�wB	�wB	��B	��B	B	B	ÖB	ĜB	ŢB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�
B	�B	�#B	�#B	�)B	�)B	�/B	�5B	�5B	�5B	�BB	�BB	�HB	�NB	�NB	�TB	�ZB	�`B	�`B	�`B	�)B	��B
jB
{B
$B
-�B
4�B
:�B
@4B
DMB
IB
PbB
TFB
W�B
\�B
abB
e�B
m�B
t�B
yrB
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
U}B
V�B
V�B
TwB
SqB
SqB
RkB
RkB
RkB
RkB
RkB
SqB
SqB
RkB
RkB
RkB
RkB
SqB
SqB
TwB
TwB
U~B
U~B
TwB
d�B
��B
i�B
g�B
��B=�B7�B;�Bi�B~^B��B�&B��B��B�B�BtB��B�jB�:B�B�.B�"B�B��B�B�B�rB�sB��B��B��B��B�B@�BGBQEBO9BN3BM,BL'BK!BIBC�BA�B>�B?�B7�B+fB�B�B�PBޠB�3B��B�kB�fB��B�HB��Br Br Be�BZ�BJ4B=�B0�B$UB�B�B
�JB
��B
�4B
�rB
��B
{oB
[�B
M^B
<�B
B	�{B	�2B	��B	�}B	��B	�rB	� B	�B	rQB	fB	^�B	W�B	U�B	MwB	8�B	&�B	\B	B		�B	�B��B�_B�B��B�rB�sB�7B� B��B��B��B��B��B��B��B�]B�@B�3B�"B�	B��B��B��B}�By�BtyBuBstBpaBo[Bo[BmOBkDBh1BaBZ�BV�BK�BL�BM�BP�BP�BR�BQ�BP�BP�BM�BL�BL�BL�BN�BO�BL�B^�Bo^BpdBv�Bz�B{�Bw�BrqBv�Bx�BsxBnYBh5BR�BP�BT�BS�BY�BQ�BS�BT�BO�BU�BR�BO�BQ�BU�BT�BP�BK�BFoBC]BEiBI�BI�BFoBFoBFoBDdBARBARB@LB?EBFpBI�BI�BO�BQ�BP�BT�BT�BR�BP�BL�BI�BEkBL�BK�BL�BK�BQ�BQ�BP�BL�BM�BT�BR�BO�BO�BN�BP�BO�BQ�BP�BO�BK�BL�BM�BN�BM�BQ�BT�BS�B\�BiABmZBrxB|�Bw�By�Bx�Bx�B{�B��B��B��B��B��B��B��B�B�B�*B�B�TB�BB�rB��B��B��B��B��B��B��B��B��B��B��B�B�B�xBΜB̏B�~BʄB�rB�rB�rB�~BˊB̐BϣBѮBҴB��B�B�(B�(B�"B�"B�B�B�	B�B�B�@B�LB�@B�@B�MB�^B�dB�qB�}B�}B�}B�}B��B��B��B	 �B		�B	3B	EB	KB	FB	LB	WB	cB	pB	pB	iB	 �B	 �B	$�B	)�B	&�B	)�B	-�B	.�B	.�B	/�B	3�B	7B	:B	<&B	>2B	@?B	CQB	FbB	J{B	N�B	W�B	W�B	V�B	U�B	X�B	[�B	^�B	_�B	eB	i1B	k>B	lCB	p[B	qaB	qaB	p[B	qaB	rhB	snB	ttB	v�B	x�B	x�B	{�B	|�B	~�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�$B	�*B	�BB	�NB	�TB	�gB	�mB	�sB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�;B	�AB	�MB	�SB	�SB	�SB	�SB	�YB	�fB	�rB	�rB	�xB	�xB	�xB	�xB	�xB	�~B	ΊB	ЖB	ЖB	ќB	ќB	ҢB	ӨB	ӨB	ӨB	յB	յB	ֻB	��B	��B	��B	��B	��B	��G�O�B	�B	�B
�B
	�B
zB
#5B
*B
05B
5�B
9�B
>�B
E�B
I�B
MB
R3B
V�B
[B
b�B
jCB
n�B
p311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.01(+/-0.004) in PSS-78.                                                                                                                                                                                        Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144222022020411442220220204114422  AO  ARCAADJP                                                                    20200619170919    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170919  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170919  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114422  IP                  G�O�G�O�G�O�                