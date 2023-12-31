CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-06-17T03:08:08Z creation; 2023-02-10T23:09:44Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.2   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     0  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \8   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     0  d   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     0  �    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  �0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  �,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  �(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0 X   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 6�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0 >T   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � ]�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0 eP   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �4   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20220617030808  20230210230944  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_217                 6810_008521_217                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @��A3	A�@��A3	A�11  @��A`A�7@��A`A�7@0�V�@0�V��d�[BE���d�[BE��11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u@   @@  @�  @�  @��R@޸RA   A��A\)A+�A>�RA_\)A�  A��A��A��A�  A�  A߮A�B   B(�B  B  B (�B'�
B/�B7�
B@  BH(�BP  BW�
B`  Bh(�Bo�
Bw�
B�
B��B��B�  B�{B�{B��B��B�  B�{B�  B�  B��B�{B�(�B�(�B�{B�  B�{B�  B�  B�{B�{B�  B�  B��B�  B��B�  B��B��B�  B��C��C  C  C  C
  C  C  C{C  C��C  C  C�C  C��C�C"
=C$  C%��C'��C*
=C,
=C.
=C0  C1�C4  C6  C7��C9�C<  C>  C?��CA�CC��CF  CH  CJ  CK�CN  CP{CR{CT  CV
=CW��CY�C\
=C^  C_��Cb{Cd  Cf  Cg��Cj  Cl  Cn  Cp  Cr  Ct
=Cv  Cw��Cy��C|  C~
=C�C�C�C�  C�  C�  C�  C���C�C�C�  C�  C���C�  C�  C�
=C�  C�  C�C�  C�  C�
=C�  C���C�C�  C���C���C�  C�\C�  C���C�  C�
=C�C���C�  C�C�
=C�C�  C�  C�C�  C���C�
=C�C���C���C���C�  C���C��C���C�C�\C�C���C�C�
=C�  C���C���C���C���C�C�
=C�  C���C���C�  C�C�C�
=C�  C���C���C���C�  C�  C�  C�  C�  C�  C�  C�C�  C�  C�C�  C���C���C�  C�C���C�C�
=C�  C�  C���C���C�  C�C�  C�C�C�
=C�
=C�  C���C�  C�
=C�C���C��C�  C�C�  C�  C�  C�C�
=C�C�  C�
=C�\C�C���C���D � D  D}qD�D}qD  D� D  D}qD�qD��D  D}qD��D� D�qDz�D�qD	}qD	�qD
}qD  D� D  D� D  D}qD��D� D�D}qD��Dz�D�D��D�qD}qD��D� D�qD}qD�D� D�qD}qD�qD� D�qDz�D�qDz�D��D}qD  D� D��D}qD�D}qD  D}qD��D��D   D � D!�D!}qD"�D"��D"�qD#}qD#�qD$}qD%  D%}qD%��D&}qD'�D'�D'�qD(z�D)�D)� D)�qD*��D+�D+�D,  D,� D-�D-� D.�D.� D.�qD/� D0�D0��D1  D1� D2�D2��D3D3��D3�qD4� D5  D5� D5�qD6z�D6�qD7��D8D8��D9  D9� D9�qD:z�D:�qD;��D<D<� D<��D=}qD=�qD>� D?�D?��D@�D@��DA�DA��DB  DB� DC�DC��DD  DD� DE  DE�DFDF��DG�DG��DH�DH� DIDI��DJ  DJ��DJ�qDK}qDL  DL�DMDM��DN�DN�DO�DO� DO�qDPz�DQ  DQ}qDQ�RDRz�DS  DSz�DS�qDT�DU�DU��DV  DV}qDW�DW��DX�DX��DYDY��DZDZ��D[�D[� D\  D\� D\�qD]��D^�D^}qD^�qD_� D`  D`}qD`�qDa}qDb�Db� Dc�Dc� Dc�qDd}qDd��De}qDf  Df��DgDg� Dg�qDh� Di  Di}qDi�qDj}qDj�qDk}qDk�qDl}qDm  Dm�Dn�Dn� Dn�qDo}qDo�qDp}qDq  Dq� Dq�qDr}qDs  Ds�Dt�Dt� Du  Du}qDv  Dv��DwDw��Dx  Dx� Dx��Dy}qDz�Dz��D{D{��D|  D|� D|�qD}� D~  D~}qD  D� D�  D�@ D�~�D���D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D���D�  D�AHD�� D���D�  D�@ D�� D��HD�  D�>�D��HD�� D�  D�@ D��HD��HD���D�@ D�� D�� D�HD�AHD�� D��HD��D�AHD��HD�� D�  D�@ D�� D�� D�  D�AHD�� D��qD���D�>�D�~�D���D�  D�AHD�� D�� D�  D�@ D�� D��HD�HD�@ D�~�D��qD���D�@ D��HD�D�  D�@ D��HD���D��qD�>�D�� D���D��qD�@ D�� D�� D�  D�@ D�� D���D���D�>�D�� D��HD���D�>�D�� D��HD�  D�>�D�~�D��HD�HD�AHD��HD���D�  D�>�D�� D���D�HD�@ D�� D�� D�  D�B�D�� D���D���D�>�D��HD�� D���D�@ D�~�D�� D�  D�>�D�� D���D�  D�B�D��HD��HD�HD�>�D�~�D�� D�  D�@ D�� D��HD�  D�@ D�~�D�� D�  D�@ D�� D�� D�HD�AHD��HD��HD�  D�@ D�� D�� D�  D�>�D��HD�� D�HD�B�D��HD��HD�HD�@ D�~�D��HD��D�@ D�~�D���D���D�@ D�� D�� D�  D�@ D�~�D���D��qD�=qD�� D��HD�HD�@ D�~�D���D���D�>�D�� D�D�HD�B�D�� D�� D�HD�@ D�� D��HD�HD�>�D�}qD��qD�  D�B�D��HD�� D�  D�AHD�� D���D��qD�@ D��HD�� D�HD�>�D�~�D���D���D�>�D�~�D��HD�HD�@ D�~�D�� D�HD�AHD��HD��HD�HD�AHD��HD��HD�  D�=qD�}qD��HD�HD�@ D�� D���D��qD�>�D�~�D�� D�HD�@ D�~�D�� D�HD�>�D�� D�� D���D�@ D��HD��HD�HD�AHD�~�D��qD���D�AHD D�� D�  D�>�DÁHD�D�HD�AHDĀ D�� D���D�>�Dŀ Dž�D��qD�>�Dƀ D��HD�  D�>�Dǀ D�� D�HD�B�DȂ�D��HD�  D�AHDɂ�D�� D���D�=qD�~�Dʾ�D�  D�>�D�~�D��HD�  D�=qD�~�D̾�D���D�@ D̀ D�� D�  D�>�D�}qDξ�D�  D�AHDπ D�� D���D�@ DЀ D�� D�  D�@ Dр D�� D���D�<)DҀ D�D�HD�B�Dӂ�D��HD���D�@ DԁHD��HD�HD�AHDՀ D�� D�  D�@ D�~�D�� D�  D�@ D�~�D�� D�HD�AHD�~�Dؾ�D�  D�@ D�~�Dپ�D�HD�AHD�~�D�� D��qD�<)D�}qD�� D�  D�@ D܁HD�� D�  D�@ D�~�D��HD�HD�>�D�~�D޾�D���D�AHD߁HD��HD�  D�AHD��HD��HD�  D�@ D�~�D�qD��qD�>�D�~�D�qD��)D�>�D� D��HD�  D�=qD� D�D�HD�=qD� D��HD�  D�AHD�HD��HD��D�AHD�~�D羸D�  D�@ D�~�D��HD�HD�AHD� D龸D���D�=qD�HD��HD��qD�=qD�~�D�qD���D�@ D�~�D�qD��qD�<)D�}qD��qD�  D�AHD� D�� D�  D�B�D�HD�qD���D�AHD�� D�D�  D�AHD�~�D�qD���D�@ D�~�D�D��qD�>�D� D��HD�  D�>�D�~�D��HD��D�@ D�~�D��qD�  D�>�D�}qD��qD���D�@ D��HD�� D��>�?�?B�\?�=q?�p�?�ff@�@��@333@E�@W
=@n{@��\@���@�
=@��\@���@�z�@�G�@���@�
=@�  @���@���AG�AA(�A�\A�A�A"�\A(��A.{A1�A7�A>{AC�
AG�AL��AS33AY��A^�RAc33AhQ�Ao\)Atz�AxQ�A~{A�=qA�p�A��A��A���A�Q�A��\A���A�Q�A��A�A��A��HA�{A�Q�A��\A�A���A�33A�A�Q�A�33A�ffA���A�33A�A�G�A�(�A�ffA���A�(�A�\)Aٙ�A�(�A�
=A�=qA���A�
=A��A��A�\)A��A�p�A�  A�=qA��B (�Bp�B�\B�
Bp�B�HB(�B	G�B
�RBQ�BB
=B(�B��B
=B��B�B
=B(�B��B33B��B��B�HB z�B!�B#33B$Q�B%B'\)B(z�B)��B+33B,��B.{B/
=B0z�B1�B3�B4��B5B733B8��B9B:�HB<(�B=B?33B@Q�BAG�BB�HBDz�BEp�BF�RBH(�BI��BJ�HBL  BM�BN�RBPQ�BQG�BR�\BT(�BUBV�RBW�
BYp�BZ�HB\Q�B]p�B^�\B`  Bap�Bb�HBc�
Bd��Bf�\Bh  Bip�BjffBk�Bmp�Bn�\Bo�Bp��Br�\Bt  Bu�Bv=qBw�ByG�Bz�RB{�B|��B~�HB�  B��\B�G�B�{B���B�G�B�  B���B��B�{B��RB�p�B�=qB���B�p�B�{B��HB��B�{B���B��B�(�B��RB�\)B�{B��HB�p�B��B��\B�\)B�  B��\B��B��B���B�33B��
B���B�p�B�{B��RB�G�B�  B���B���B�{B��RB��B�Q�B��HB�p�B�=qB���B�p�B�(�B�
=B��B�=qB���B��B�Q�B���B��B�{B���B���B�=qB��RB�\)B�(�B��HB�p�B�  B��RB��B�(�B���B�\)B�{B���B�p�B�  B��\B�G�B�  B��\B�
=B��B�(�B��HB�\)B�B�=qB��HB�p�B�B�{B��\B��B�p�B��B�  B�z�B��HB�
=B�G�B��B�(�B�Q�B\B��HB�G�BîB��B�(�B�ffB���B�G�BŅBŮB�{B�z�B���B���B�G�BǮB�{B�Q�Bȏ\B��HB�G�Bə�B��
B�{B�z�B���B�G�B�p�BˮB�{B�z�B��HB�
=B�G�BͮB�{B�ffBΏ\B���B�33BϮB��B�{B�ffB��HB�G�BхB�B�{Bҏ\B���B�G�B�p�B�B�(�Bԏ\B���B�G�BՅB��
B�=qB֣�B���B��BׅB�  B�ffBظRB���B�G�BٮB�{B�z�BڸRB�
=B�\)B��
B�Q�B܏\B��HB��B�p�B��B�ffBޣ�B��HB�G�B�B�(�B�ffB��B��B�B��B�=qB�z�B��HB�G�B�B�(�B�ffB�RB�
=B噚B�  B�(�B�\B�
=B�p�B��
B�  B�Q�B���B�G�B陚B��
B�(�B�z�B�
=B�p�B�B�  B�z�B���B�G�B�B�{B�\B���B�33B�B�{B�z�B���B��B�p�B�  B�z�B���B��B�p�B��B�ffB��RB�
=B��B�  B�Q�B��\B���B�\)B��B�Q�B���B���B�G�B��B�(�B��RB���B�G�B��B�(�B���B���B�33B���B�{B���B��HB�33B��C 
=C G�C p�C �\C C
=C=qCffC�C�RC��C33C\)C�C�C�C(�CffC�C�C�HC{CQ�C�\CC�C
=CQ�C�\CC�HC{CG�C�CC�C{C=qCz�C�C�C(�CQ�Cz�C�C�C	�C	\)C	z�C	��C	�HC
�C
\)C
�\C
�RC
�HC
=C=qCz�C�RC�C{C=qCffC��C�
C{CG�CffC��CC
=C=qCffC�\C�RC��C33Cp�C��CC�C{C\)C��C��C�C{CQ�C�CC��C{C=qCz�C�RC�C{C=qC\)C�\C��C
=C33CQ�Cp�C��C�
C{CG�C\)C�C�C�HC�CQ�C�C��C��C��C(�CQ�C�\C�RC�
C  C33CffC��C��C��C
=C33C\)C�\C��C�C
=C=qCp�C�RC�C{C(�CQ�C�\C��C  C33C\)C�C�C�
C{CQ�C�\C�RC�HC{CG�C�\C��C  C(�C\)C�\C�
C {C Q�C �C �RC �HC!�C!ffC!�\C!C!�C"�C"ffC"�C"��C"��C#(�C#ffC#��C#�HC${C$=qC$ffC$��C$��C%33C%ffC%�\C%��C%��C&33C&p�C&�RC&��C'(�C'\)C'�C'�RC(  C(G�C(�C(�RC(��C)(�C)ffC)��C)��C*{C*\)C*��C*��C*��C+33C+p�C+�RC+��C,33C,ffC,��C,�
C-
=C-G�C-�\C-�
C.{C.Q�C.z�C.�C.�C/(�C/p�C/�C/��C033C0p�C0�C0�C1(�C1Q�C1�\C1�RC1��C233C2ffC2��C2�HC3�C3ffC3��C3�HC4�C4Q�C4�C4�RC4�C5(�C5ffC5�C5�C6(�C6ffC6�\C6��C7
=C7G�C7�\C7�
C8�C8\)C8��C8�HC9�C9\)C9��C9�
C:{C:Q�C:��C:�HC;33C;z�C;�RC;��C<=qC<p�C<��C<�HC=(�C=\)C=�C=�C>33C>p�C>�RC>��C?33C?p�C?��C?�HC@�C@\)C@��C@�CA33CAffCA��CA�
CB{CB\)CB��CB�HCC(�CC\)CC��CC��CD
=CDG�CD�CD��CE{CE\)CE��CE�
CF{CFG�CFz�CF�RCG  CGG�CG�\CGCG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                            ?u@   @@  @�  @�  @��R@޸RA   A��A\)A+�A>�RA_\)A�  A��A��A��A�  A�  A߮A�B   B(�B  B  B (�B'�
B/�B7�
B@  BH(�BP  BW�
B`  Bh(�Bo�
Bw�
B�
B��B��B�  B�{B�{B��B��B�  B�{B�  B�  B��B�{B�(�B�(�B�{B�  B�{B�  B�  B�{B�{B�  B�  B��B�  B��B�  B��B��B�  B��C��C  C  C  C
  C  C  C{C  C��C  C  C�C  C��C�C"
=C$  C%��C'��C*
=C,
=C.
=C0  C1�C4  C6  C7��C9�C<  C>  C?��CA�CC��CF  CH  CJ  CK�CN  CP{CR{CT  CV
=CW��CY�C\
=C^  C_��Cb{Cd  Cf  Cg��Cj  Cl  Cn  Cp  Cr  Ct
=Cv  Cw��Cy��C|  C~
=C�C�C�C�  C�  C�  C�  C���C�C�C�  C�  C���C�  C�  C�
=C�  C�  C�C�  C�  C�
=C�  C���C�C�  C���C���C�  C�\C�  C���C�  C�
=C�C���C�  C�C�
=C�C�  C�  C�C�  C���C�
=C�C���C���C���C�  C���C��C���C�C�\C�C���C�C�
=C�  C���C���C���C���C�C�
=C�  C���C���C�  C�C�C�
=C�  C���C���C���C�  C�  C�  C�  C�  C�  C�  C�C�  C�  C�C�  C���C���C�  C�C���C�C�
=C�  C�  C���C���C�  C�C�  C�C�C�
=C�
=C�  C���C�  C�
=C�C���C��C�  C�C�  C�  C�  C�C�
=C�C�  C�
=C�\C�C���C���D � D  D}qD�D}qD  D� D  D}qD�qD��D  D}qD��D� D�qDz�D�qD	}qD	�qD
}qD  D� D  D� D  D}qD��D� D�D}qD��Dz�D�D��D�qD}qD��D� D�qD}qD�D� D�qD}qD�qD� D�qDz�D�qDz�D��D}qD  D� D��D}qD�D}qD  D}qD��D��D   D � D!�D!}qD"�D"��D"�qD#}qD#�qD$}qD%  D%}qD%��D&}qD'�D'�D'�qD(z�D)�D)� D)�qD*��D+�D+�D,  D,� D-�D-� D.�D.� D.�qD/� D0�D0��D1  D1� D2�D2��D3D3��D3�qD4� D5  D5� D5�qD6z�D6�qD7��D8D8��D9  D9� D9�qD:z�D:�qD;��D<D<� D<��D=}qD=�qD>� D?�D?��D@�D@��DA�DA��DB  DB� DC�DC��DD  DD� DE  DE�DFDF��DG�DG��DH�DH� DIDI��DJ  DJ��DJ�qDK}qDL  DL�DMDM��DN�DN�DO�DO� DO�qDPz�DQ  DQ}qDQ�RDRz�DS  DSz�DS�qDT�DU�DU��DV  DV}qDW�DW��DX�DX��DYDY��DZDZ��D[�D[� D\  D\� D\�qD]��D^�D^}qD^�qD_� D`  D`}qD`�qDa}qDb�Db� Dc�Dc� Dc�qDd}qDd��De}qDf  Df��DgDg� Dg�qDh� Di  Di}qDi�qDj}qDj�qDk}qDk�qDl}qDm  Dm�Dn�Dn� Dn�qDo}qDo�qDp}qDq  Dq� Dq�qDr}qDs  Ds�Dt�Dt� Du  Du}qDv  Dv��DwDw��Dx  Dx� Dx��Dy}qDz�Dz��D{D{��D|  D|� D|�qD}� D~  D~}qD  D� D�  D�@ D�~�D���D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D���D�  D�AHD�� D���D�  D�@ D�� D��HD�  D�>�D��HD�� D�  D�@ D��HD��HD���D�@ D�� D�� D�HD�AHD�� D��HD��D�AHD��HD�� D�  D�@ D�� D�� D�  D�AHD�� D��qD���D�>�D�~�D���D�  D�AHD�� D�� D�  D�@ D�� D��HD�HD�@ D�~�D��qD���D�@ D��HD�D�  D�@ D��HD���D��qD�>�D�� D���D��qD�@ D�� D�� D�  D�@ D�� D���D���D�>�D�� D��HD���D�>�D�� D��HD�  D�>�D�~�D��HD�HD�AHD��HD���D�  D�>�D�� D���D�HD�@ D�� D�� D�  D�B�D�� D���D���D�>�D��HD�� D���D�@ D�~�D�� D�  D�>�D�� D���D�  D�B�D��HD��HD�HD�>�D�~�D�� D�  D�@ D�� D��HD�  D�@ D�~�D�� D�  D�@ D�� D�� D�HD�AHD��HD��HD�  D�@ D�� D�� D�  D�>�D��HD�� D�HD�B�D��HD��HD�HD�@ D�~�D��HD��D�@ D�~�D���D���D�@ D�� D�� D�  D�@ D�~�D���D��qD�=qD�� D��HD�HD�@ D�~�D���D���D�>�D�� D�D�HD�B�D�� D�� D�HD�@ D�� D��HD�HD�>�D�}qD��qD�  D�B�D��HD�� D�  D�AHD�� D���D��qD�@ D��HD�� D�HD�>�D�~�D���D���D�>�D�~�D��HD�HD�@ D�~�D�� D�HD�AHD��HD��HD�HD�AHD��HD��HD�  D�=qD�}qD��HD�HD�@ D�� D���D��qD�>�D�~�D�� D�HD�@ D�~�D�� D�HD�>�D�� D�� D���D�@ D��HD��HD�HD�AHD�~�D��qD���D�AHD D�� D�  D�>�DÁHD�D�HD�AHDĀ D�� D���D�>�Dŀ Dž�D��qD�>�Dƀ D��HD�  D�>�Dǀ D�� D�HD�B�DȂ�D��HD�  D�AHDɂ�D�� D���D�=qD�~�Dʾ�D�  D�>�D�~�D��HD�  D�=qD�~�D̾�D���D�@ D̀ D�� D�  D�>�D�}qDξ�D�  D�AHDπ D�� D���D�@ DЀ D�� D�  D�@ Dр D�� D���D�<)DҀ D�D�HD�B�Dӂ�D��HD���D�@ DԁHD��HD�HD�AHDՀ D�� D�  D�@ D�~�D�� D�  D�@ D�~�D�� D�HD�AHD�~�Dؾ�D�  D�@ D�~�Dپ�D�HD�AHD�~�D�� D��qD�<)D�}qD�� D�  D�@ D܁HD�� D�  D�@ D�~�D��HD�HD�>�D�~�D޾�D���D�AHD߁HD��HD�  D�AHD��HD��HD�  D�@ D�~�D�qD��qD�>�D�~�D�qD��)D�>�D� D��HD�  D�=qD� D�D�HD�=qD� D��HD�  D�AHD�HD��HD��D�AHD�~�D羸D�  D�@ D�~�D��HD�HD�AHD� D龸D���D�=qD�HD��HD��qD�=qD�~�D�qD���D�@ D�~�D�qD��qD�<)D�}qD��qD�  D�AHD� D�� D�  D�B�D�HD�qD���D�AHD�� D�D�  D�AHD�~�D�qD���D�@ D�~�D�D��qD�>�D� D��HD�  D�>�D�~�D��HD��D�@ D�~�D��qD�  D�>�D�}qD��qD���D�@ D��HD�� G�O�>�?�?B�\?�=q?�p�?�ff@�@��@333@E�@W
=@n{@��\@���@�
=@��\@���@�z�@�G�@���@�
=@�  @���@���AG�AA(�A�\A�A�A"�\A(��A.{A1�A7�A>{AC�
AG�AL��AS33AY��A^�RAc33AhQ�Ao\)Atz�AxQ�A~{A�=qA�p�A��A��A���A�Q�A��\A���A�Q�A��A�A��A��HA�{A�Q�A��\A�A���A�33A�A�Q�A�33A�ffA���A�33A�A�G�A�(�A�ffA���A�(�A�\)Aٙ�A�(�A�
=A�=qA���A�
=A��A��A�\)A��A�p�A�  A�=qA��B (�Bp�B�\B�
Bp�B�HB(�B	G�B
�RBQ�BB
=B(�B��B
=B��B�B
=B(�B��B33B��B��B�HB z�B!�B#33B$Q�B%B'\)B(z�B)��B+33B,��B.{B/
=B0z�B1�B3�B4��B5B733B8��B9B:�HB<(�B=B?33B@Q�BAG�BB�HBDz�BEp�BF�RBH(�BI��BJ�HBL  BM�BN�RBPQ�BQG�BR�\BT(�BUBV�RBW�
BYp�BZ�HB\Q�B]p�B^�\B`  Bap�Bb�HBc�
Bd��Bf�\Bh  Bip�BjffBk�Bmp�Bn�\Bo�Bp��Br�\Bt  Bu�Bv=qBw�ByG�Bz�RB{�B|��B~�HB�  B��\B�G�B�{B���B�G�B�  B���B��B�{B��RB�p�B�=qB���B�p�B�{B��HB��B�{B���B��B�(�B��RB�\)B�{B��HB�p�B��B��\B�\)B�  B��\B��B��B���B�33B��
B���B�p�B�{B��RB�G�B�  B���B���B�{B��RB��B�Q�B��HB�p�B�=qB���B�p�B�(�B�
=B��B�=qB���B��B�Q�B���B��B�{B���B���B�=qB��RB�\)B�(�B��HB�p�B�  B��RB��B�(�B���B�\)B�{B���B�p�B�  B��\B�G�B�  B��\B�
=B��B�(�B��HB�\)B�B�=qB��HB�p�B�B�{B��\B��B�p�B��B�  B�z�B��HB�
=B�G�B��B�(�B�Q�B\B��HB�G�BîB��B�(�B�ffB���B�G�BŅBŮB�{B�z�B���B���B�G�BǮB�{B�Q�Bȏ\B��HB�G�Bə�B��
B�{B�z�B���B�G�B�p�BˮB�{B�z�B��HB�
=B�G�BͮB�{B�ffBΏ\B���B�33BϮB��B�{B�ffB��HB�G�BхB�B�{Bҏ\B���B�G�B�p�B�B�(�Bԏ\B���B�G�BՅB��
B�=qB֣�B���B��BׅB�  B�ffBظRB���B�G�BٮB�{B�z�BڸRB�
=B�\)B��
B�Q�B܏\B��HB��B�p�B��B�ffBޣ�B��HB�G�B�B�(�B�ffB��B��B�B��B�=qB�z�B��HB�G�B�B�(�B�ffB�RB�
=B噚B�  B�(�B�\B�
=B�p�B��
B�  B�Q�B���B�G�B陚B��
B�(�B�z�B�
=B�p�B�B�  B�z�B���B�G�B�B�{B�\B���B�33B�B�{B�z�B���B��B�p�B�  B�z�B���B��B�p�B��B�ffB��RB�
=B��B�  B�Q�B��\B���B�\)B��B�Q�B���B���B�G�B��B�(�B��RB���B�G�B��B�(�B���B���B�33B���B�{B���B��HB�33B��C 
=C G�C p�C �\C C
=C=qCffC�C�RC��C33C\)C�C�C�C(�CffC�C�C�HC{CQ�C�\CC�C
=CQ�C�\CC�HC{CG�C�CC�C{C=qCz�C�C�C(�CQ�Cz�C�C�C	�C	\)C	z�C	��C	�HC
�C
\)C
�\C
�RC
�HC
=C=qCz�C�RC�C{C=qCffC��C�
C{CG�CffC��CC
=C=qCffC�\C�RC��C33Cp�C��CC�C{C\)C��C��C�C{CQ�C�CC��C{C=qCz�C�RC�C{C=qC\)C�\C��C
=C33CQ�Cp�C��C�
C{CG�C\)C�C�C�HC�CQ�C�C��C��C��C(�CQ�C�\C�RC�
C  C33CffC��C��C��C
=C33C\)C�\C��C�C
=C=qCp�C�RC�C{C(�CQ�C�\C��C  C33C\)C�C�C�
C{CQ�C�\C�RC�HC{CG�C�\C��C  C(�C\)C�\C�
C {C Q�C �C �RC �HC!�C!ffC!�\C!C!�C"�C"ffC"�C"��C"��C#(�C#ffC#��C#�HC${C$=qC$ffC$��C$��C%33C%ffC%�\C%��C%��C&33C&p�C&�RC&��C'(�C'\)C'�C'�RC(  C(G�C(�C(�RC(��C)(�C)ffC)��C)��C*{C*\)C*��C*��C*��C+33C+p�C+�RC+��C,33C,ffC,��C,�
C-
=C-G�C-�\C-�
C.{C.Q�C.z�C.�C.�C/(�C/p�C/�C/��C033C0p�C0�C0�C1(�C1Q�C1�\C1�RC1��C233C2ffC2��C2�HC3�C3ffC3��C3�HC4�C4Q�C4�C4�RC4�C5(�C5ffC5�C5�C6(�C6ffC6�\C6��C7
=C7G�C7�\C7�
C8�C8\)C8��C8�HC9�C9\)C9��C9�
C:{C:Q�C:��C:�HC;33C;z�C;�RC;��C<=qC<p�C<��C<�HC=(�C=\)C=�C=�C>33C>p�C>�RC>��C?33C?p�C?��C?�HC@�C@\)C@��C@�CA33CAffCA��CA�
CB{CB\)CB��CB�HCC(�CC\)CC��CC��CD
=CDG�CD�CD��CE{CE\)CE��CE�
CF{CFG�CFz�CF�RCG  CGG�CG�\CGCG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                            @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ƨA���Aհ!AնFAէ�Aՙ�AՕ�A՗�A�|�A�jA�hsA�hsA�K�A� �A��A�oA�
=A�%A�  A���A���A���A��A��A��A��yA��/A�ȴAԲ-Aԛ�A�p�A�(�AӃA�M�A�+A��AҍPA��`AыDA�t�A�bNA�K�A�5?A�JA���A�AжFAв-AН�AЅA�E�A�1'A��A��`A�I�AΑhA�VA��TA�ƨA�M�A˟�A��TA�ffA���A�n�A��`Aȥ�A�l�A���A�S�A�Aś�A���A�VA��AöFA�A�ZA���A��A��A�%A���A��wA���A��yA��yA�JA��A�O�A��PA��TA���A�7LA�z�A���A�JA���A�
=A��hA�%A�&�A��FA�1'A���A�^5A��A���A���A��A��\A�bNA��A�oA��A�~�A��#A�ƨA�-A�(�A��mA��HA�;dA�+A��!A�p�A�1A�l�A{?}Ay�Ay
=Aw|�Au7LAr��Ak�Ai��Af�!Ac��AaK�A_�A]�A[�AV�ASAR�AP��AM|�AJ9XAH�yAGt�AF��AC��AB�9A@-A=�mA<��A:��A5ƨA2{A0(�A/�^A/C�A.�/A.-A,5?A+7LA*�A)"�A'�-A'S�A&��A%��A$bNA$(�A"Q�A!+A 5?A�A`BAO�A&�A�A�A�yA��A�At�A�TAĜA��A�AdZA��AM�A�A~�A`BA��A1'AJA|�A��A
�!A
n�A
=qA	S�A�9AM�A�wA��Ax�AhsA7LAC�AC�A+A&�A��A-A�AAȴAbNA(�Ax�AI�A��AVA ��A �`A ȴA �@�t�@�v�@�{@��^@���@�X@���@��`@���@���@���@��D@�bN@�Z@�Q�@�1'@�\)@�"�@��#@�  @��@�v�@���@��@�X@��@�1'@�@��T@��@��`@@�+@���@��;@�t�@�R@�-@�b@�dZ@��H@�$�@���@�bN@�@�K�@��H@�@���@�Ĝ@�I�@���@�C�@��@�x�@܋D@��@۝�@�t�@�
=@�&�@؛�@��@�K�@ְ!@Չ7@��@�S�@�
=@���@�^5@�?}@�(�@�K�@�n�@���@�7L@̣�@�Q�@�1@˅@�l�@�S�@�33@�"�@�ȴ@ʗ�@��@��@ȓu@�j@�9X@�|�@���@Ƈ+@�J@Ł@�Q�@Å@§�@�@�O�@�7L@�%@���@�1'@�t�@�@��H@�ȴ@���@��\@�~�@���@�x�@�/@���@��@��@�l�@�+@�t�@�dZ@���@���@��@�Ĝ@�Q�@�(�@�b@���@�|�@���@��@��-@��@��@��P@�K�@�@���@�ff@�@��h@�O�@��@�b@�ƨ@��@���@��@�\)@�"�@�E�@���@�`B@�O�@�?}@�/@��@�%@��/@��u@�Z@��m@���@�;d@�o@��y@���@�n�@�V@�E�@��#@�O�@��@��j@�(�@���@�o@��\@��@��^@���@�hs@�&�@��D@��m@��;@��P@�+@�$�@���@��h@�G�@�&�@�Z@�  @���@��@�|�@�l�@�S�@�33@�
=@��@�~�@��@�@��-@�x�@�?}@��@�Ĝ@�j@�ƨ@�C�@�
=@��!@�{@���@��@�/@��@� �@���@��m@��
@���@�33@���@��+@�5?@���@���@�G�@���@��@�b@��m@��P@�;d@��@�M�@�@��@��@��@��@�@��@���@��@�1@��@�C�@��H@��+@�5?@��@�@���@�p�@�&�@��`@�Ĝ@��u@�Z@�Z@�Q�@�A�@��m@���@�C�@�o@���@�5?@�@���@�x�@�hs@�X@�O�@�7L@�V@���@�j@�b@��
@�t�@���@�E�@�-@�@��T@���@��@�r�@�A�@� �@�  @��m@���@�ƨ@��P@��@��@�ȴ@��!@���@���@���@���@���@���@���@�M�@�$�@�J@��@��#@��-@��7@�p�@�?}@�Ĝ@�Z@\)@~E�@}�-@|�/@{�m@{�F@{t�@z��@z��@z~�@zM�@yhs@y�@x�`@x�9@xbN@x1'@w�;@wl�@w;d@w�@w
=@v�@v��@v��@v�+@vv�@vV@v5?@u@u�@t�j@t�D@t(�@s�m@sƨ@s�@sS�@so@r��@r~�@r^5@q�#@qX@q�@pA�@o|�@o\)@n�@m�T@m/@l��@l9X@kƨ@k�@k33@k"�@k@j��@j�\@jn�@j=q@i��@i�^@ix�@i%@h��@h�`@hb@g��@g�P@g|�@g|�@g|�@gl�@g\)@gK�@g;d@g+@f�y@f��@e�T@e�@dz�@d(�@c��@c33@b��@a��@`r�@`b@`  @`  @_��@^��@^{@]�-@]��@]�h@]�h@]�@]?}@\�@\�D@\I�@[�m@[�
@[�
@[�
@[��@[33@Z�@Z~�@Y�#@Yx�@X��@W�@WK�@VE�@U��@U/@UV@T�@T��@T�j@Tz�@T�@S��@S@R��@R�!@R��@R�\@Q��@Q&�@P�9@PQ�@O�w@O�@N�@Nff@N@M�h@M�-@MO�@L�@LZ@K@J��@J^5@I�^@I&�@H�9@Hr�@Hb@G�w@G+@F�+@F{@EO�@D�j@D(�@C��@C�F@CdZ@C"�@B�@B�\@B^5@BM�@B-@A�^@AG�@@�9@@��@@�@?�;@?�@?|�@?K�@?
=@>��@>v�@>V@>E�@>E�@>@=��@=p�@=/@<��@<�@<��@<z�@<(�@<�@;�
@;��@;S�@:��@:-@:J@9�^@9&�@8Ĝ@8�@8b@7�w@7\)@7+@7
=@6��@6�y@6ȴ@6��@6$�@5��@5�-@5�h@5�h@5p�@4��@4�j@4��@4j@3�@3"�@2�@2��@2~�@2=q@2-@2J@1��@1��@1G�@0Ĝ@0��@01'@/��@/l�@/
=@.�@.��@.v�@.v�@.V@.$�@-�h@-/@-V@-V@,�@,�j@,��@,Z@,1@,�@,�@+ƨ@+��@+��@+t�@+33@*n�@)�#@)�^@)��@)x�@)X@)7L@)&�@)�@)%@(�`@(�9@(��@(�u@(1'@'\)@&ȴ@&V@&{@%��@%��@%p�@%?}@$�@$�j@$Z@$9X@$1@#��@#S�@#33@#o@"�@"n�@"M�@"=q@"=q@"J@!�#@!��@!�7@!hs@!G�@!7L@!&�@ ��@ ��@ �u@ �@ r�@ bN@ b@�;@l�@�@�y@��@�+@v�@v�@V@{@@�@�-@p�@p�@`B@O�@`B@/@�@�@�j@��@Z@(�@�m@ƨ@�F@�F@�F@�F@�@dZ@C�@"�@�H@�!@�\@M�@-@��@�@��@hs@7L@�`@�9@b@��@��@�@|�@\)@�@�y@ȴ@��@ff@E�@{@�@�T@��@O�@V@��@�D@(�@��@�m@�F@dZ@C�@"�@@�@�H@��@��@�\@-@J@�#@��@X@7L@&�@�@��@�9@1'@ �@  @�;@�w@�@|�@|�@\)@K�@;d@�@��@�R@��@�+@�+@V@$�@@�@�T@��@��@�-@p�@O�@�@��@�@��@�@z�A�ȴA�ȴA�A�A�ȴA�A���AռjAմ9AլAլAնFAռjAհ!AլAա�A՝�Aՙ�AՕ�A՛�AՑhAՕ�A՗�A՛�A՗�AՋDAՇ+A�~�A�jA�n�A�ffA�jA�ffA�dZA�^5A�`BA�n�A�p�A�l�A�VA�bNA�VA�C�A�7LA�5?A�+A��A��A��A� �A��A��A��A�$�A��A�{A��A��A�{A�VA�bA�bA�JA�1A�
=A�JA�JA�
=A�1A�
=A�
=A�%A�A�  A�A�A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��mA��yA��A��mA��;A��;A��HA��/A��A��#A��
A���A�ƨA�ĜAԾwAԼjAԸRAԲ-AԴ9AԶFAԮAԥ�Aԥ�Aԥ�Aԡ�Aԗ�AԓuAԓuAԑhAԇ+A�v�A�jA�hsA�`BA�S�A�?}A�=qA�(�A��A�VA���A���A�r�A�XA�VA�VA�S�A�O�A�K�A�K�A�O�A�K�A�I�A�E�A�;dA�1'A�$�A�VA�%A�  A��A��yA��TA��HA���AҸRAҥ�A҇+A�t�A�ffA�S�A�&�A���A��
A�ĜAѰ!Aї�AэPAч+Aч+AыDAщ7A�~�A�z�A�x�A�v�A�p�A�n�A�jA�jA�ffA�^5A�ZA�ZA�XA�O�A�M�A�M�A�K�A�G�A�A�A�=qA�;dA�9XA�1'A�-A�-A�(�A��A�bA�A��A��/A��
A���A���A���A���A���A�ĜA�ĜA�ĜA���AиRAк^AиRAв-Aв-AжFAжFAд9Aв-Aв-AжFAд9AЮAХ�AС�AУ�AП�AЗ�AЕ�AЗ�AЕ�AЏ\A�~�A�z�A�n�A�^5A�M�A�I�A�E�A�=qA�;dA�9XA�7LA�5?A�33A�33A�/A�&�A�"�A�$�A��A��A��A��A��A��A�oA�oA�oA�
=A���A��mA��/A���A���Aϴ9Aϰ!Aϰ!Aϧ�Aϗ�Aχ+A�~�A�r�A�ZA�?}A�1'A��A�A��yAξwAήAΩ�AΣ�AΗ�AΓuAΕ�AΕ�A΍PAΉ7AΉ7A΍PA·+A�x�A�p�A�jA�hsA�dZA�^5A�VA�VA�VA�S�A�G�A�?}A�5?A�+A�(�A�"�A��A�oA�1A�  A���A��TA���Aͧ�A�~�A�VA�5?A� �A�
=A��A��#A�ȴA̲-Ḁ�A̝�A̓uÁA�z�A�t�A�ffA�^5A�XA�XA�VA�Q�A�G�A�;dA�33A�+A��A���A��A��/A���A˼jAˡ�A˃A�\)A�;dA�33A�$�A�oA�%A���A��A��mA��TA���A�ƨA���A�A���AʸRAʩ�AʑhA�t�A�hsA�Q�A�C�A�;dA�7LA�7LA�5?A�(�A��A��A�VA�  A��A��mA��;A��A���A�ƨAɺ^Aɰ!AɬAɗ�A�~�A�hsA�K�A�7LA�"�A��A�JA�%A���A���A��yA��mA��`A���A�ĜAȾwAȾwAȸRAȰ!Aȧ�Aȥ�Aȥ�Aȣ�Aȟ�Aș�Aȗ�Aȕ�Aȏ\Aȇ+AȅAȇ+AȅA�z�A�jA�XA�E�A��A�  A��yA��HA���A�AǼjAǲ-Aǣ�AǙ�AǓuAǏ\AǃA�p�A�`BA�VA�M�A�G�A�?}A�1'A�+A�-A�(�A�"�A��A��A�{A�1A���A��/A�ȴAƮA�l�A�&�A���Aś�A�l�A�C�A�?}A�?}A�?}A�=qA�5?A� �A�1A�  A��A��mA��`A��`A��;A�ƨAĴ9Aġ�Aĉ7A�ffA�`BA�S�A�=qA� �A�%A���A��A��A��mA��HA��A��
A���A���A�ƨA�AüjAüjAüjAüjAüjAú^AöFAò-Að!Aç�AÓuA�n�A�Q�A�(�A���A��A°!A�t�A�5?A��;A�ȴA�ƨA��A��A�XA�G�A�C�A�?}A�=qA�-A�%A��A��A��`A��
A���A���A�ffA�E�A�A�A�A�A�?}A�=qA�;dA�9XA�;dA�33A��A���A��yA��;A��A��
A���A���A���A��A�Q�A�JA��`A��hA�`BA�S�A��A��A��wA��DA�A��A��/A�A���A���A�x�A�\)A�VA�Q�A�S�A�Q�A�C�A�(�A��A�{A�%A��^A��FA�  A��+A�
=A�+A��HA���A�z�A�&�A��yA�ffA���A�v�A� �A�5?A���A��7A�A�A�A��PA�G�A�$�A��A�{A�1A���A��yA���A��RA��A���A�XA�=qA�+A�JA��A���A��-A���A��DA�p�A�\)A�Q�A�G�A�7LA�JA��mA�ƨA��A���A��uA�z�A�XA�?}A� �A���A�ȴA���A�t�A�hsA�^5A�ZA�XA�S�A�G�A�E�A�A�A� �A�oA�%A���A���A���A�XA���A��A��A��yA��mA��A��`A��TA��;A��/A��/A��A���A��^A���A���A��A�z�A�bNA�O�A�K�A�E�A�?}A�9XA�33A�+A�&�A�"�A��A��A�
=A��mA���A�Q�A��A��A�G�A��A���A��jA��\A�p�A�dZA�S�A�I�A�=qA�;dA�33A�(�A��A��A�  A��yA�ĜA��hA�ZA���A��A��/A�ȴA��FA���A�\)A�"�A�
=A���A���A��yA��`A��
A���A��wA��-A���A���A��PA�M�A��yA��hA�E�A�$�A���A��/A���A��!A��uA�x�A�Q�A�=qA��A�
=A�A���A��A��yA��HA���A��FA���A���A��+A�z�A�t�A�p�A�ffA�O�A�/A�%A��HA��#A��#A�ĜA��jA��RA���A���A��A�|�A�|�A�z�A�n�A�hsA�bNG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                            A�ƨA���Aհ!AնFAէ�Aՙ�AՕ�A՗�A�|�A�jA�hsA�hsA�K�A� �A��A�oA�
=A�%A�  A���A���A���A��A��A��A��yA��/A�ȴAԲ-Aԛ�A�p�A�(�AӃA�M�A�+A��AҍPA��`AыDA�t�A�bNA�K�A�5?A�JA���A�AжFAв-AН�AЅA�E�A�1'A��A��`A�I�AΑhA�VA��TA�ƨA�M�A˟�A��TA�ffA���A�n�A��`Aȥ�A�l�A���A�S�A�Aś�A���A�VA��AöFA�A�ZA���A��A��A�%A���A��wA���A��yA��yA�JA��A�O�A��PA��TA���A�7LA�z�A���A�JA���A�
=A��hA�%A�&�A��FA�1'A���A�^5A��A���A���A��A��\A�bNA��A�oA��A�~�A��#A�ƨA�-A�(�A��mA��HA�;dA�+A��!A�p�A�1A�l�A{?}Ay�Ay
=Aw|�Au7LAr��Ak�Ai��Af�!Ac��AaK�A_�A]�A[�AV�ASAR�AP��AM|�AJ9XAH�yAGt�AF��AC��AB�9A@-A=�mA<��A:��A5ƨA2{A0(�A/�^A/C�A.�/A.-A,5?A+7LA*�A)"�A'�-A'S�A&��A%��A$bNA$(�A"Q�A!+A 5?A�A`BAO�A&�A�A�A�yA��A�At�A�TAĜA��A�AdZA��AM�A�A~�A`BA��A1'AJA|�A��A
�!A
n�A
=qA	S�A�9AM�A�wA��Ax�AhsA7LAC�AC�A+A&�A��A-A�AAȴAbNA(�Ax�AI�A��AVA ��A �`A ȴA �@�t�@�v�@�{@��^@���@�X@���@��`@���@���@���@��D@�bN@�Z@�Q�@�1'@�\)@�"�@��#@�  @��@�v�@���@��@�X@��@�1'@�@��T@��@��`@@�+@���@��;@�t�@�R@�-@�b@�dZ@��H@�$�@���@�bN@�@�K�@��H@�@���@�Ĝ@�I�@���@�C�@��@�x�@܋D@��@۝�@�t�@�
=@�&�@؛�@��@�K�@ְ!@Չ7@��@�S�@�
=@���@�^5@�?}@�(�@�K�@�n�@���@�7L@̣�@�Q�@�1@˅@�l�@�S�@�33@�"�@�ȴ@ʗ�@��@��@ȓu@�j@�9X@�|�@���@Ƈ+@�J@Ł@�Q�@Å@§�@�@�O�@�7L@�%@���@�1'@�t�@�@��H@�ȴ@���@��\@�~�@���@�x�@�/@���@��@��@�l�@�+@�t�@�dZ@���@���@��@�Ĝ@�Q�@�(�@�b@���@�|�@���@��@��-@��@��@��P@�K�@�@���@�ff@�@��h@�O�@��@�b@�ƨ@��@���@��@�\)@�"�@�E�@���@�`B@�O�@�?}@�/@��@�%@��/@��u@�Z@��m@���@�;d@�o@��y@���@�n�@�V@�E�@��#@�O�@��@��j@�(�@���@�o@��\@��@��^@���@�hs@�&�@��D@��m@��;@��P@�+@�$�@���@��h@�G�@�&�@�Z@�  @���@��@�|�@�l�@�S�@�33@�
=@��@�~�@��@�@��-@�x�@�?}@��@�Ĝ@�j@�ƨ@�C�@�
=@��!@�{@���@��@�/@��@� �@���@��m@��
@���@�33@���@��+@�5?@���@���@�G�@���@��@�b@��m@��P@�;d@��@�M�@�@��@��@��@��@�@��@���@��@�1@��@�C�@��H@��+@�5?@��@�@���@�p�@�&�@��`@�Ĝ@��u@�Z@�Z@�Q�@�A�@��m@���@�C�@�o@���@�5?@�@���@�x�@�hs@�X@�O�@�7L@�V@���@�j@�b@��
@�t�@���@�E�@�-@�@��T@���@��@�r�@�A�@� �@�  @��m@���@�ƨ@��P@��@��@�ȴ@��!@���@���@���@���@���@���@���@�M�@�$�@�J@��@��#@��-@��7@�p�@�?}@�Ĝ@�Z@\)@~E�@}�-@|�/@{�m@{�F@{t�@z��@z��@z~�@zM�@yhs@y�@x�`@x�9@xbN@x1'@w�;@wl�@w;d@w�@w
=@v�@v��@v��@v�+@vv�@vV@v5?@u@u�@t�j@t�D@t(�@s�m@sƨ@s�@sS�@so@r��@r~�@r^5@q�#@qX@q�@pA�@o|�@o\)@n�@m�T@m/@l��@l9X@kƨ@k�@k33@k"�@k@j��@j�\@jn�@j=q@i��@i�^@ix�@i%@h��@h�`@hb@g��@g�P@g|�@g|�@g|�@gl�@g\)@gK�@g;d@g+@f�y@f��@e�T@e�@dz�@d(�@c��@c33@b��@a��@`r�@`b@`  @`  @_��@^��@^{@]�-@]��@]�h@]�h@]�@]?}@\�@\�D@\I�@[�m@[�
@[�
@[�
@[��@[33@Z�@Z~�@Y�#@Yx�@X��@W�@WK�@VE�@U��@U/@UV@T�@T��@T�j@Tz�@T�@S��@S@R��@R�!@R��@R�\@Q��@Q&�@P�9@PQ�@O�w@O�@N�@Nff@N@M�h@M�-@MO�@L�@LZ@K@J��@J^5@I�^@I&�@H�9@Hr�@Hb@G�w@G+@F�+@F{@EO�@D�j@D(�@C��@C�F@CdZ@C"�@B�@B�\@B^5@BM�@B-@A�^@AG�@@�9@@��@@�@?�;@?�@?|�@?K�@?
=@>��@>v�@>V@>E�@>E�@>@=��@=p�@=/@<��@<�@<��@<z�@<(�@<�@;�
@;��@;S�@:��@:-@:J@9�^@9&�@8Ĝ@8�@8b@7�w@7\)@7+@7
=@6��@6�y@6ȴ@6��@6$�@5��@5�-@5�h@5�h@5p�@4��@4�j@4��@4j@3�@3"�@2�@2��@2~�@2=q@2-@2J@1��@1��@1G�@0Ĝ@0��@01'@/��@/l�@/
=@.�@.��@.v�@.v�@.V@.$�@-�h@-/@-V@-V@,�@,�j@,��@,Z@,1@,�@,�@+ƨ@+��@+��@+t�@+33@*n�@)�#@)�^@)��@)x�@)X@)7L@)&�@)�@)%@(�`@(�9@(��@(�u@(1'@'\)@&ȴ@&V@&{@%��@%��@%p�@%?}@$�@$�j@$Z@$9X@$1@#��@#S�@#33@#o@"�@"n�@"M�@"=q@"=q@"J@!�#@!��@!�7@!hs@!G�@!7L@!&�@ ��@ ��@ �u@ �@ r�@ bN@ b@�;@l�@�@�y@��@�+@v�@v�@V@{@@�@�-@p�@p�@`B@O�@`B@/@�@�@�j@��@Z@(�@�m@ƨ@�F@�F@�F@�F@�@dZ@C�@"�@�H@�!@�\@M�@-@��@�@��@hs@7L@�`@�9@b@��@��@�@|�@\)@�@�y@ȴ@��@ff@E�@{@�@�T@��@O�@V@��@�D@(�@��@�m@�F@dZ@C�@"�@@�@�H@��@��@�\@-@J@�#@��@X@7L@&�@�@��@�9@1'@ �@  @�;@�w@�@|�@|�@\)@K�@;d@�@��@�R@��@�+@�+@V@$�@@�@�T@��@��@�-@p�@O�@�@��@�@��@�G�O�A�ȴA�ȴA�A�A�ȴA�A���AռjAմ9AլAլAնFAռjAհ!AլAա�A՝�Aՙ�AՕ�A՛�AՑhAՕ�A՗�A՛�A՗�AՋDAՇ+A�~�A�jA�n�A�ffA�jA�ffA�dZA�^5A�`BA�n�A�p�A�l�A�VA�bNA�VA�C�A�7LA�5?A�+A��A��A��A� �A��A��A��A�$�A��A�{A��A��A�{A�VA�bA�bA�JA�1A�
=A�JA�JA�
=A�1A�
=A�
=A�%A�A�  A�A�A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��mA��yA��A��mA��;A��;A��HA��/A��A��#A��
A���A�ƨA�ĜAԾwAԼjAԸRAԲ-AԴ9AԶFAԮAԥ�Aԥ�Aԥ�Aԡ�Aԗ�AԓuAԓuAԑhAԇ+A�v�A�jA�hsA�`BA�S�A�?}A�=qA�(�A��A�VA���A���A�r�A�XA�VA�VA�S�A�O�A�K�A�K�A�O�A�K�A�I�A�E�A�;dA�1'A�$�A�VA�%A�  A��A��yA��TA��HA���AҸRAҥ�A҇+A�t�A�ffA�S�A�&�A���A��
A�ĜAѰ!Aї�AэPAч+Aч+AыDAщ7A�~�A�z�A�x�A�v�A�p�A�n�A�jA�jA�ffA�^5A�ZA�ZA�XA�O�A�M�A�M�A�K�A�G�A�A�A�=qA�;dA�9XA�1'A�-A�-A�(�A��A�bA�A��A��/A��
A���A���A���A���A���A�ĜA�ĜA�ĜA���AиRAк^AиRAв-Aв-AжFAжFAд9Aв-Aв-AжFAд9AЮAХ�AС�AУ�AП�AЗ�AЕ�AЗ�AЕ�AЏ\A�~�A�z�A�n�A�^5A�M�A�I�A�E�A�=qA�;dA�9XA�7LA�5?A�33A�33A�/A�&�A�"�A�$�A��A��A��A��A��A��A�oA�oA�oA�
=A���A��mA��/A���A���Aϴ9Aϰ!Aϰ!Aϧ�Aϗ�Aχ+A�~�A�r�A�ZA�?}A�1'A��A�A��yAξwAήAΩ�AΣ�AΗ�AΓuAΕ�AΕ�A΍PAΉ7AΉ7A΍PA·+A�x�A�p�A�jA�hsA�dZA�^5A�VA�VA�VA�S�A�G�A�?}A�5?A�+A�(�A�"�A��A�oA�1A�  A���A��TA���Aͧ�A�~�A�VA�5?A� �A�
=A��A��#A�ȴA̲-Ḁ�A̝�A̓uÁA�z�A�t�A�ffA�^5A�XA�XA�VA�Q�A�G�A�;dA�33A�+A��A���A��A��/A���A˼jAˡ�A˃A�\)A�;dA�33A�$�A�oA�%A���A��A��mA��TA���A�ƨA���A�A���AʸRAʩ�AʑhA�t�A�hsA�Q�A�C�A�;dA�7LA�7LA�5?A�(�A��A��A�VA�  A��A��mA��;A��A���A�ƨAɺ^Aɰ!AɬAɗ�A�~�A�hsA�K�A�7LA�"�A��A�JA�%A���A���A��yA��mA��`A���A�ĜAȾwAȾwAȸRAȰ!Aȧ�Aȥ�Aȥ�Aȣ�Aȟ�Aș�Aȗ�Aȕ�Aȏ\Aȇ+AȅAȇ+AȅA�z�A�jA�XA�E�A��A�  A��yA��HA���A�AǼjAǲ-Aǣ�AǙ�AǓuAǏ\AǃA�p�A�`BA�VA�M�A�G�A�?}A�1'A�+A�-A�(�A�"�A��A��A�{A�1A���A��/A�ȴAƮA�l�A�&�A���Aś�A�l�A�C�A�?}A�?}A�?}A�=qA�5?A� �A�1A�  A��A��mA��`A��`A��;A�ƨAĴ9Aġ�Aĉ7A�ffA�`BA�S�A�=qA� �A�%A���A��A��A��mA��HA��A��
A���A���A�ƨA�AüjAüjAüjAüjAüjAú^AöFAò-Að!Aç�AÓuA�n�A�Q�A�(�A���A��A°!A�t�A�5?A��;A�ȴA�ƨA��A��A�XA�G�A�C�A�?}A�=qA�-A�%A��A��A��`A��
A���A���A�ffA�E�A�A�A�A�A�?}A�=qA�;dA�9XA�;dA�33A��A���A��yA��;A��A��
A���A���A���A��A�Q�A�JA��`A��hA�`BA�S�A��A��A��wA��DA�A��A��/A�A���A���A�x�A�\)A�VA�Q�A�S�A�Q�A�C�A�(�A��A�{A�%A��^A��FA�  A��+A�
=A�+A��HA���A�z�A�&�A��yA�ffA���A�v�A� �A�5?A���A��7A�A�A�A��PA�G�A�$�A��A�{A�1A���A��yA���A��RA��A���A�XA�=qA�+A�JA��A���A��-A���A��DA�p�A�\)A�Q�A�G�A�7LA�JA��mA�ƨA��A���A��uA�z�A�XA�?}A� �A���A�ȴA���A�t�A�hsA�^5A�ZA�XA�S�A�G�A�E�A�A�A� �A�oA�%A���A���A���A�XA���A��A��A��yA��mA��A��`A��TA��;A��/A��/A��A���A��^A���A���A��A�z�A�bNA�O�A�K�A�E�A�?}A�9XA�33A�+A�&�A�"�A��A��A�
=A��mA���A�Q�A��A��A�G�A��A���A��jA��\A�p�A�dZA�S�A�I�A�=qA�;dA�33A�(�A��A��A�  A��yA�ĜA��hA�ZA���A��A��/A�ȴA��FA���A�\)A�"�A�
=A���A���A��yA��`A��
A���A��wA��-A���A���A��PA�M�A��yA��hA�E�A�$�A���A��/A���A��!A��uA�x�A�Q�A�=qA��A�
=A�A���A��A��yA��HA���A��FA���A���A��+A�z�A�t�A�p�A�ffA�O�A�/A�%A��HA��#A��#A�ĜA��jA��RA���A���A��A�|�A�|�A�z�A�n�A�hsA�bNG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                            ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B|PB}VB|�B{B|�B|B{�B{JB|�B{BzxBz�B{�BzxBzBy�BzBzByrByrByrBy�By	Bx�ByrBy�Bz�B|PB~(BcB��B�SB�xB��B��B�4B�B��B��B�kB��B�xB�IB�!B�'B��B��B��B��B��B�:B��B��B��B��B�VB��B��B��B��B��B�aB�zB��B��B�gB�XB�B��B�mB��B1B4B�B \B �B,=B2�B8RB9XB=�BD3BA�BS&B_pB[�B_;B_�B_pB[�B`�B]/B]dB[#B^�BWsBR BPBM�BK)B?�B7�B3hB/OB($B"�BCB	B�cB�B�B��B�zB��B��Bn�BT�B:�B \B\B�B
��B
�;B
�TB
��B
�tB
��B
g�B
=qB
.IB
'�B
VB
�B	��B	ԕB	�B	�CB	�=B	�SB	{B	o�B	g�B	Z�B	G�B	EB	A�B	EmB	.}B	%B	�B	�B	�B	_B	�B	�B	�B		7B��B��B	�B	�B	�B	�B	�B	)*B	(�B	($B	1�B	:�B	;�B	B'B	T,B	k�B	zB	��B	�MB	�%B	�B	�eB	�B	��B	�CB	��B	�_B	�oB	��B	�uB	��B	~�B	}�B	|�B	|�B	�B	��B	��B	��B	�7B	�fB	�YB	�B	��B	�B	��B	��B	��B	�@B	�:B	��B	�+B	��B	�eB	�7B	��B	�B	��B	��B	��B	��B	��B	�3B	��B	��B	�[B	��B	��B	��B	��B	��B	��B	��B	�XB	�0B	��B	��B	��B	�B	��B	�FB	��B	�?B	�B	��B	��B	�B	��B	�B	��B	�wB	��B	��B	�[B	�3B	� B	��B	�UB	��B	�B	�B	�<B	��B	��B	��B	��B	�B	��B	�B	��B	�B	�B	�'B	��B	��B	�B	�6B	�B	͟B	͟B	͟B	�B	�BB	�}B	�vB	��B	ΥB	�NB	�}B	ѷB	ҽB	ҽB	ҽB	�TB	�&B	��B	ٴB	ںB	یB	یB	�B	��B	�dB	�)B	�B	�;B	�;B	�B	��B	��B	��B	�B	�vB	�B	�|B	�B	�B	�|B	��B	�|B	�B	�B	��B	�`B	�`B	�B	��B	�B	�fB	�B	�`B	�ZB	�B	��B	�B	�mB	�B	�B	�B	��B	�B	�)B	�WB	�B	�B	�"B	�B	�)B	�B	�WB	��B	�)B	�B	�B	� B	�B	�B	��B	�%B	��B	�fB	��B	�JB	��B	�xB	�xB	��B	��B	��B	�B	��B	�.B
 �B
B
B
oB
B
B
�B
AB
GB
�B
�B
�B
�B
B
B
�B
�B
�B
	�B

rB
xB
B
�B
�B
�B
�B
�B
B
\B
�B
(B
�B
(B
\B
(B
�B
�B
.B
bB
hB
oB
uB
@B
FB
�B
B
{B
�B
B
YB
$B
�B
+B
�B
�B
=B
�B
�B
�B
�B
�B
�B
�B
VB
VB
�B
�B
�B
!B
 �B
 �B
 �B
 �B
!-B
!bB
!�B
!�B
"4B
#�B
#�B
#�B
%B
&�B
&B
%�B
&�B
'�B
'�B
($B
'�B
'�B
(XB
(�B
)�B
)�B
)�B
)�B
*�B
+6B
,=B
+�B
-CB
-B
-�B
-�B
.�B
/�B
0UB
0UB
0!B
0UB
0�B
1'B
2�B
2�B
33B
49B
5B
6B
6zB
7�B
7�B
7�B
8RB
8�B
8�B
8�B
8�B
8B
8�B
8�B
8�B
8�B
9$B
9�B
9�B
9�B
9�B
:*B
:�B
:�B
;0B
;�B
;�B
<B
<B
<6B
<jB
<6B
<�B
<�B
<�B
<�B
>wB
>BB
>wB
>�B
>�B
?}B
@�B
@�B
@�B
AUB
AUB
A�B
A�B
A�B
B'B
B�B
B�B
C-B
C�B
C�B
C�B
C�B
CaB
CaB
CaB
C-B
C�B
C�B
C�B
D�B
EB
E�B
FB
E�B
FB
F�B
GzB
H�B
IB
I�B
J�B
J�B
K)B
L0B
L�B
L�B
MB
L�B
NB
M�B
NB
NB
NpB
N�B
OB
OBB
OvB
OvB
OBB
O�B
O�B
O�B
O�B
O�B
O�B
O�B
PHB
QB
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
RTB
RTB
R�B
R�B
R�B
S�B
S�B
S�B
TaB
S�B
S�B
S[B
S�B
S�B
S�B
S�B
S�B
T,B
S�B
TaB
TaB
T�B
T�B
UgB
U�B
V9B
VB
V�B
X�B
YB
YB
YB
YB
YB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
[�B
[#B
[WB
[�B
[�B
]/B
^5B
^B
^B
]�B
^B
_B
_pB
_�B
_�B
_�B
_�B
_�B
_�B
`B
`B
`�B
`�B
`�B
`vB
`BB
`vB
`vB
`vB
`�B
a|B
aHB
a�B
a�B
a�B
b�B
cTB
cTB
b�B
c B
c B
b�B
b�B
b�B
cTB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c B
c B
b�B
b�B
cTB
c�B
c�B
d�B
e�B
f2B
e�B
f2B
f�B
f2B
f�B
g8B
h�B
h�B
h�B
iB
h�B
jB
i�B
i�B
jKB
kQB
kQB
k�B
l"B
l"B
l�B
l�B
m�B
m�B
m�B
m�B
n/B
ncB
n�B
n/B
ncB
o B
o B
o5B
oiB
o�B
p;B
o�B
pB
pB
o�B
p;B
p;B
p�B
p�B
poB
p�B
p�B
qB
qAB
qvB
qvB
qvB
q�B
r�B
sB
r�B
s�B
tB
tTB
t�B
t�B
u%B
u�B
u�B
u�B
u�B
u�B
v+B
v�B
w2B
w�B
wfB
w�B
w�B
w�B
x8B
x8B
xB
xlB
y�B
yrB
y�B
y�B
zB
zDB
zDB
zDB
zxB
z�B
{B
{�B
{�B
{�B
|B
|B
{�B
{�B
|PB
|PB
|PB
|�B
|�B
}�B
}�B
}�B
}�B
}�B
~]B
~�B
�B
.B
�B
�B
�4B
� B
�4B
� B
� B
�4B
�;B
�oB
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
�GB
��B
��B
��B
�B
��B
�MB
�MB
��B
��B
�B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�+B
�_B
�_B
�_B
��B
��B
��B
��B
��B
��B
�1B
��B
�1B
�fB
��B
�7B
�lB
��B
��B
��B
��B
��B
��B
�	B
�=B
�	B
��B
��B
��B
��B
��B
��B
�B
�B
�DB
�xB
�xB
��B
��B
�~B
�JB
�~B
�~B
�JB
�~B
��B
��B
��B
��B
�PB
��B
��B
��B
��B
�"B
��B
��B
��B
��B
�\B
�\B
��B
�bB
�bB
��B
� B
��B
��B
��B
�B
�:B
��B
��B
��B
��B
��B
�@B
�@B
��B
��B
�B
�{B
�{B
�FB
��B
��B
��B
�MB
��B
��B
��B
��B
��B
��B
��B
�B
�SB
��B
��B
��B
��B
��B
�$B
�YB
��B
��B
��B
��B
��B
��B
�+B
�_B
�_B
�_B
��B
��B
�1B
��B
�eB
��B
��B
��B
��B
�B
�7B
�B
�7B
�B
�B
�kB
��B
��B
��B
�	B
�=B
�qB
�Bz�B{B}"B|PB}"B}�B~(B{JB� B|�B|�BzDB{JB|B|�B�4B|PB}VB{B{ByrB}�B}�By�B|�B{�B��B{�BzB{B{JBz�Bz�B{JB~�Bz�BxB{JB{�B}�Bx�B~(B~(B|BzxBy�B|�Bz�Bz�BxBzxBzxBx�BxB{JBz�By>Bx�By�B{ByrBy>BzDB{BzBy	By	By�Bz�BzBy>By�Bz�BzxBx�Bx�By�Bz�By	Bx�By�Bz�By>Bx8Bx�BzDBy�BxlBzDBzDBx8Bx�BzxBzBx�BxlBzxBz�BxlBxlBx�BzByrBxlBw�BxlBzBzDBx�BxBx�BzBzxBzDBx�Bx�Bz�Bz�By�Bx�By�B{�BzxByrBz�B|B{Bz�B{B}"B}VB}�B|�B}VB~]B}�B|PB~]B�B.B~]BcB�B�iB.B�B��B�B�{B�;B�uB��B��B��B��B�{B�+B��B�4B�MB�DB�lB�lB��B��B�B�JB�B�JB�~B��B��B��B��B��B��B�bB��B�hB��B�bB�B�oB��B�MB�hB�hB�uB�1B��B��B�SB�SB�B��B�$B��B�SB�YB��B�B�7B�kB�7B�=B�xB��B��B��B��B�=B��B��B�B�CB�qB��B�IB�B�B�xB��B��B��B��B��B�'B�VB�hB��B�\B�-B�bB��B�!B��B��B�bB��B��B��B�-B�'B�hB��B��B��B��B��B�bB�'B��B�4B��B�-B�'B�bB��B��B�\B�'B��B��B�hB��B�hB��B�:B��B�hB��B��B�B�tB��B�hB��B�FB�@B��B��B�nB��B��B�bB��B��B�4B�-B�4B�B��B��B�@B��B�FB��B��B�:B��B�B��B��B�hB��B��B�:B��B�B��B��B��B��B��B�'B��B�B��B��B��B��B��B��B��B�'B�IB��B��B��B�B�B��B�IB�B�~B��B�	B�CB�B�B��B��B��B��B�4B�bB��B��B��B��B��B��B��B��B�eB�*B��B�_B��B�B�XB�B�_B��B��B��B��B��B�eB��B��B��B��B�B�CB�IB�wB�UB�UB�9B��B��B��B��B�[B��B�3B��B�[B�hB��B�3B��B��B��B��B��B��B�FB��B�LB��B��B�FB��B��B�XB�B��B�RB�*B��B��B�LB��B��B�^B�*B�XB��B��B�BB��B�OB�OB��B��B�UB��B�[B�3B��B��B��B�zB�BǮB��B�RB˒B˒B�XB��B�XB˒BʌB��BʌB�0B�dB��B��B��B�B�B� B�?BרB�B�KB�5B�BݘB��B�BB�|B�NB��B��B�B��B��B�sB�B��B�B�B�B�DB�KB��B��B��B�QB��B�5B�B�vB��B��B	�BBVB.B�BBBxBBoB�B.BoBuBhB�B�B�B�B�B�B�B$B�BB �B#B �B�B�B�B \B �B �B \B �B �B"�B"4B!�B �B \B�B�B �B �B 'B 'B#B%�B%zB+B+6B)_B+�B/�B3�B:^B/OB,�B3�B0�B6zB3hB2-B1�B0�B49B6FB6B4�B5tB4�B7B:^BA�B<�B7LB6�B7B7�B8RB7�B7B8RB<�B>BB;�B:�B7LB6�B5�B8RB:�B;�B@�B:�B?�BF?B>�B>BF�BA�BEBC-BV�B@�BCaBB�BC�B@�BC�B?HB?B>�B=�B<jB<�B>B:�B7B5tB@�B`vBm)BL�BUgBb�BGzBF�BS�BJ�BR�BU2BQ�B]�BQ�B{�BVmBW?B_�Ba�Be,Ba�B\�BZ�B[#B[�B[�B\�B]/BY�BZQB^�Bb�B]dB\�B`BB_pB`�B_;B[�B^�B`�B_;B]dB^B^�BaB`BbNB_B\�B[�B_�BaHB`�B_�BaHBa�Ba|BZ�B[#B[#B\]B[�B\�B_BY�BZB_;B[#B[�B^BbBc�Bk�Bc�B]/B_;B_B^jB\�B\�B\]B\�B\�B\�B]/B`B]�B[�B[WB]�B\�B^jB^5B\�B[#B[�B[WBZ�BZ�BY�BX�BYKBW�BYB^jBb�Ba�Bp;B_B\]BW�B[WBZ�BV9BT�BS&BS�BR�BS�BRTBRTBQNBP}BP�BR�BQ�BT�BT�BZ�BRTBM6BMBM6BK�BMBV9BR�BLdBJ�BK�BK�BJXBK^BIBI�BIRBGzBGBJ#BR�BRTBK�BF�B>BB?}B;�B:*B<B9�B;dB:*B8B9�B6FB5�B5�B49B2aB2�B5B7�B3�B2�B2aB0!B/�B.IB/�B1'B2�B0!B1[B)�B(XB/B&LB&�B)*B(�B)�B%B$tB#nB$tB"4B"hG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022061703080820220617030808IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022071610012120220716100121QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022071610012120220716100121QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194520230210131945IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                